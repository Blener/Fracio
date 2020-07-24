// Copyright 2018-2019 Fabulous contributors. See LICENSE.md for license.
namespace Fracio

open System
open Fabulous
open Fabulous.XamarinForms
open LiteDB
open LiteDB.FSharp
open Xamarin.Forms
open System.Linq

module App =
    type Ingredient =
        { Id: Guid
          Name: string }
        static member New() =
            { Id = Guid.NewGuid()
              Name = "" }
    
    type DetailedIngredient =
        { Id: Guid
          IngredientId: Guid
          Measure: float
          Price: float }
        static member New ingredientId () =
            { Id = Guid.NewGuid()
              IngredientId = ingredientId
              Measure = 0.
              Price = 0. }
    
    type Modules =
        | IngredientsPage
        | FractionPage
    
    type Model = 
      { Ingredients: Ingredient list
        DetailedIngredients: DetailedIngredient list
        FractionedBy: int
        CurrentPage: Modules }

    type Msg =
        | NavigateTo of Modules
        
        | IngredientsLoaded of Ingredient list
        | CreateIngredient
        | SetIngredientName of Guid * string
        | IngredientSaved
        
        | AddIngredientToDetailed of Ingredient
        | SetMeasure of Guid * float
        | SetPrice of Guid * float
        | SetFractionedBy of int
        
    type CmdMsg =
        | LoadIngredientsCmd
        | SaveIngredientCmd of Ingredient

    let mapCmdToMsg (sqlPath:string) (cmdMsg:CmdMsg) : Cmd<Msg> =
        let mapper = FSharpBsonMapper()
        use db = new LiteDatabase(sqlPath, mapper)
        match cmdMsg with
        | LoadIngredientsCmd ->
            async {
                do! Async.SwitchToThreadPool()
                let collection = db.GetCollection<Ingredient>()
                return collection.FindAll() |> List.ofSeq |> IngredientsLoaded
            } |> Cmd.ofAsyncMsg
        | SaveIngredientCmd ingredient ->
            async {
                do! Async.SwitchToThreadPool()
                let collection = db.GetCollection<Ingredient>()
                do collection.Upsert ingredient |> ignore
                return IngredientSaved
            } |> Cmd.ofAsyncMsg
    
    let initModel =
        { Ingredients = [  ]
          DetailedIngredients = [  ]
          FractionedBy = 1
          CurrentPage = IngredientsPage }

    let init () =
        initModel, [ LoadIngredientsCmd ]

    let update msg model =        
        match msg with
        | NavigateTo page ->
            { model with CurrentPage = page }, []
        
        | IngredientsLoaded ingredients ->
            { model with Ingredients = ingredients }, []
        | CreateIngredient ->
            let newIngredient = Ingredient.New()
            { model with Ingredients = newIngredient::model.Ingredients }, []
        | SetIngredientName (id, name) ->
            let ingredient = model.Ingredients |> List.find (fun x -> x.Id = id)
            let updatedIngredient = { ingredient with Name = name }
            let updatedIngredients = model.Ingredients |> List.map (fun x -> if x.Id = id then updatedIngredient else x)
            { model with Ingredients = updatedIngredients }, [ SaveIngredientCmd updatedIngredient ]
        | IngredientSaved ->
            model, []
            
        | AddIngredientToDetailed ingredient ->
            let newDetails =
                let alreadyAdded = model.DetailedIngredients |> List.exists (fun x -> x.IngredientId = ingredient.Id)
                match alreadyAdded with
                | true ->
                    model.DetailedIngredients
                | false ->
                    let newDetailed = DetailedIngredient.New ingredient.Id ()
                    newDetailed::model.DetailedIngredients
            { model with DetailedIngredients = newDetails }, [  ]
        | SetMeasure (detailedId, measure) ->
            let detail = model.DetailedIngredients |> List.find (fun x -> x.Id = detailedId)
            let updatedDetail = { detail with Measure = measure }
            let updatedDetails = model.DetailedIngredients |> List.map (fun x -> if x.Id = detailedId then updatedDetail else x)
            { model with DetailedIngredients = updatedDetails }, [  ]
        | SetPrice (detailedId, price) ->
            let detail = model.DetailedIngredients |> List.find (fun x -> x.Id = detailedId)
            let updatedDetail = { detail with Price = price }
            let updatedDetails = model.DetailedIngredients |> List.map (fun x -> if x.Id = detailedId then updatedDetail else x)
            { model with DetailedIngredients = updatedDetails }, [  ]
        | SetFractionedBy value ->
            { model with FractionedBy = value }, [  ]

    let view (model: Model) dispatch =
        View.MasterDetailPage(
            master = View.ContentPage(
                title = "Fracionador de ingredientes",
                icon = Image.ImagePath "hamburger.png",
                content = View.ScrollView(
                    content = View.StackLayout(
                        children =
                            [
                                View.Button(
                                    text = "Ingredientes",
                                    command = (fun () -> NavigateTo IngredientsPage |> dispatch),
                                    backgroundColor = Color.Transparent
                                )
                                View.Button(
                                    text = "Fracionar",
                                    command = (fun () -> NavigateTo FractionPage |> dispatch),
                                    backgroundColor = Color.Transparent
                                )
                            ]
                    )
                )
            ),
            detail = View.NavigationPage(
                pages = [
                    match model.CurrentPage with
                    | IngredientsPage ->
                        View.ContentPage(
                            title = "Ingredientes",
                            content = View.ScrollView(
                                content = View.StackLayout(
                                    children = [
                                        View.Button(
                                            text = "Criar ingrediente",
                                            command = (fun () -> CreateIngredient |> dispatch)
                                        )
                                        View.FlexLayout(
                                            wrap = FlexWrap.Wrap,
                                            justifyContent = FlexJustify.SpaceEvenly,
                                            children = [
                                                for i in model.Ingredients do
                                                    View.Frame(
                                                        content =
                                                            View.StackLayout(
                                                                children = [
                                                                    View.Entry(
                                                                        text = i.Name,
                                                                        placeholder = "Nome",
                                                                        textChanged = (fun args -> SetIngredientName (i.Id, args.NewTextValue) |> dispatch)
                                                                    )
                                                                ]
                                                            )
                                                    )
                                            ]
                                        )
                                    ]
                                )
                            )
                        )
                    | FractionPage ->
                        let fraction = float model.FractionedBy
                        View.ContentPage(
                            title = "Fracionar ingredientes",
                            content = View.ScrollView(
                                content = View.Grid(
                                    padding = Thickness(8.),
                                    rowdefs = [ Auto; Absolute 30.; Star; Absolute 50. ],
                                    coldefs = [ Star ],
                                    children = [
                                        View.Label("Selecione os ingredientes para compor o fracionamento").Row(0)
                                        View.CollectionView(
                                            itemsLayout = LinearItemsLayout(ItemsLayoutOrientation.Horizontal, ItemSpacing = 7.),
                                            selectionMode = SelectionMode.Single,
                                            items = [
                                                let availableIngredients = model.Ingredients |> List.filter (fun x -> not <| String.IsNullOrEmpty(x.Name))
                                                for i in availableIngredients do
                                                    View.Label(
                                                        text = i.Name,
                                                        padding = Thickness(2.),
                                                        backgroundColor = Color.LightGray,
                                                        fontAttributes = FontAttributes.Bold,
                                                        tag = i
                                                    )
                                            ],
                                            selectionChanged = (fun (_, (currentItems: ViewElement list option)) ->
                                                match currentItems |> Option.bind(Seq.tryHead) with
                                                | None -> ()
                                                | Some item ->
                                                    let ingredient = item.TryGetTag<Ingredient>().Value
                                                    dispatch (AddIngredientToDetailed ingredient) )
                                        ).Row(1)
                                        let detailedRows = [ Auto ]@[ for _ in 0..model.DetailedIngredients.Length * 2 -> Auto ]
                                        let detailedCols = [ Auto; Auto; Auto; Star ]
                                        View.Grid(
                                            rowdefs = detailedRows,
                                            coldefs = detailedCols,
                                            children = [
                                                View.Label(
                                                    text = "Ingrediente",
                                                    verticalTextAlignment = TextAlignment.Center
                                                ).Column(0).Row(0)
                                                View.Label(
                                                    text = "Receita",
                                                    verticalTextAlignment = TextAlignment.Center
                                                ).Column(2).Row(0)
                                                View.StackLayout(
                                                    orientation = StackOrientation.Horizontal,
                                                    horizontalOptions = LayoutOptions.End,
                                                    children = [
                                                        View.Label(
                                                            text = "1/",
                                                            verticalTextAlignment = TextAlignment.Center
                                                        )
                                                        View.Entry(
                                                            text = string model.FractionedBy,
                                                            keyboard = Keyboard.Numeric,
                                                            textChanged = (fun args ->
                                                                match Int32.TryParse(args.NewTextValue) with
                                                                | false, _ ->
                                                                    ()
                                                                | true, parsed ->
                                                                    match parsed with
                                                                    | 0 ->
                                                                        SetFractionedBy 1 |> dispatch
                                                                    | _ ->
                                                                        SetFractionedBy parsed |> dispatch)
                                                        )
                                                    ]
                                                ).Column(3).Row(0)
                                                let mutable curRow = 1
                                                for i in model.DetailedIngredients do
                                                    let ingredient = model.Ingredients |> List.find (fun x -> x.Id = i.IngredientId)
                                                    let index = model.DetailedIngredients |> List.findIndex (fun x -> x.Id = i.Id)
                                                    let backgroundColor = match index % 2 = 0 with | false -> Color.White | true -> Color.FromHex("#F3F3F3")
                                                    View.BoxView(
                                                        color = backgroundColor
                                                    ).Row(curRow).ColumnSpan(detailedCols.Length).RowSpan(2)
                                                    View.Label(
                                                        text = ingredient.Name,
                                                        verticalTextAlignment = TextAlignment.Center
                                                    ).Column(0).Row(curRow).RowSpan(2)
                                                    View.Label(
                                                        text = "Medida:",
                                                        verticalTextAlignment = TextAlignment.Center
                                                    ).Column(1).Row(curRow)
                                                    View.Label(
                                                        text = "Preço:",
                                                        verticalTextAlignment = TextAlignment.Center
                                                    ).Column(1).Row(curRow + 1)
                                                    View.Entry(
                                                        text = string i.Measure,
                                                        keyboard = Keyboard.Numeric,
                                                        textChanged = (fun args ->
                                                            match Double.TryParse(args.NewTextValue) with
                                                            | false, _ ->
                                                                ()
                                                            | true, parsed ->
                                                                SetMeasure (i.Id, parsed) |> dispatch)
                                                    ).Column(2).Row(curRow)
                                                    View.Entry(
                                                        text = string i.Price,
                                                        keyboard = Keyboard.Numeric,
                                                        textChanged = (fun args -> 
                                                            match Double.TryParse(args.NewTextValue) with
                                                            | false, _ ->
                                                                ()
                                                            | true, parsed ->
                                                                SetPrice (i.Id, parsed) |> dispatch)
                                                    ).Column(2).Row(curRow + 1)
                                                    View.Label(
                                                        text = sprintf "%.3f" (i.Measure / fraction),
                                                        verticalTextAlignment = TextAlignment.Center,
                                                        horizontalOptions = LayoutOptions.End
                                                    ).Column(3).Row(curRow)
                                                    View.Label(
                                                        text = sprintf "%.3f" (i.Price / fraction),
                                                        verticalTextAlignment = TextAlignment.Center,
                                                        horizontalOptions = LayoutOptions.End
                                                    ).Column(3).Row(curRow + 1)
                                                    curRow <- curRow + 2
                                            ]
                                        ).Row(2)
                                        let recipePriceTotal = model.DetailedIngredients |> List.sumBy (fun x -> x.Price)
                                        let fractionedPriceTotal = model.DetailedIngredients |> List.sumBy (fun x -> x.Price / fraction)
                                        View.StackLayout(
                                            backgroundColor = Color.LightBlue,
                                            orientation = StackOrientation.Horizontal,
                                            children = [
                                                View.Label(
                                                    text = sprintf "Total Receita: %.2f" recipePriceTotal,
                                                    fontAttributes = FontAttributes.Bold,
                                                    verticalTextAlignment = TextAlignment.Center
                                                )
                                                View.Label(
                                                    text = sprintf "Total Fracionado: %.2f" fractionedPriceTotal,
                                                    fontAttributes = FontAttributes.Bold,
                                                    verticalTextAlignment = TextAlignment.Center
                                                )
                                            ]
                                        ).Row(3)
                                    ]
                                )
                            )
                        )
                ]
            )
        )

    // Note, this declaration is needed if you enable LiveUpdate
    let program init update cmdToMsg =
        Program.mkProgramWithCmdMsg init update view cmdToMsg

type App (sqlPath) as app = 
    inherit Application ()
    
    let init = App.init
    let update = App.update
    let mapCmdToMsg = App.mapCmdToMsg sqlPath
    
    let runner = 
        App.program init update mapCmdToMsg
#if DEBUG
        |> Program.withConsoleTrace
#endif
        |> XamarinFormsProgram.run app

#if DEBUG
    // Uncomment this line to enable live update in debug mode. 
    // See https://fsprojects.github.io/Fabulous/Fabulous.XamarinForms/tools.html#live-update for further  instructions.
    //
    //do runner.EnableLiveUpdate()
#endif    

    // Uncomment this code to save the application state to app.Properties using Newtonsoft.Json
    // See https://fsprojects.github.io/Fabulous/Fabulous.XamarinForms/models.html#saving-application-state for further  instructions.
#if APPSAVE
    let modelId = "model"
    override __.OnSleep() = 

        let json = Newtonsoft.Json.JsonConvert.SerializeObject(runner.CurrentModel)
        Console.WriteLine("OnSleep: saving model into app.Properties, json = {0}", json)

        app.Properties.[modelId] <- json

    override __.OnResume() = 
        Console.WriteLine "OnResume: checking for model in app.Properties"
        try 
            match app.Properties.TryGetValue modelId with
            | true, (:? string as json) -> 

                Console.WriteLine("OnResume: restoring model from app.Properties, json = {0}", json)
                let model = Newtonsoft.Json.JsonConvert.DeserializeObject<App.Model>(json)

                Console.WriteLine("OnResume: restoring model from app.Properties, model = {0}", (sprintf "%0A" model))
                runner.SetCurrentModel (model, Cmd.none)

            | _ -> ()
        with ex -> 
            App.program.onError("Error while restoring model found in app.Properties", ex)

    override this.OnStart() = 
        Console.WriteLine "OnStart: using same logic as OnResume()"
        this.OnResume()
#endif


