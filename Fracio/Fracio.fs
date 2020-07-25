// Copyright 2018-2019 Fabulous contributors. See LICENSE.md for license.
namespace Fracio

open System
open Fabulous
open Fabulous.XamarinForms
open LiteDB
open LiteDB.FSharp
open Xamarin.Forms

module App =
    type Ingredient =
        { Id: Guid
          Name: string
          Price: float }
        static member New() =
            { Id = Guid.NewGuid()
              Name = ""
              Price = 0. }
    
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
        | SaveIngredient of Guid
        | DeleteIngredient of Ingredient
        | SetIngredientName of Guid * string
        | SetIngredientPrice of Guid * float
        | IngredientSaved
        | IngredientDeleted
        
        | AddIngredientToDetailed of Ingredient
        | DeleteDetailedIngredient of Guid
        | SetMeasure of Guid * float
        | SetPrice of Guid * float
        | SetFractionedBy of int
        
    type CmdMsg =
        | LoadIngredientsCmd
        | SaveIngredientCmd of Ingredient
        | DeleteIngredientCmd of Guid

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
        | DeleteIngredientCmd id ->
            async {
                do! Async.SwitchToThreadPool()
                let collection = db.GetCollection<Ingredient>()
                do collection.Delete(BsonValue id) |> ignore
                return IngredientDeleted
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
        | SaveIngredient id ->
            let ingredient = model.Ingredients |> List.find (fun x -> x.Id = id)
            model, [ SaveIngredientCmd ingredient ]
        | DeleteIngredient ingredient ->
            let newIngredients =
                model.Ingredients |> List.filter (fun x -> not (x.Equals ingredient))
            let newDetailedIngredients =
                model.DetailedIngredients |> List.filter (fun x -> not (x.IngredientId = ingredient.Id))
            { model with Ingredients = newIngredients; DetailedIngredients = newDetailedIngredients }, [ DeleteIngredientCmd ingredient.Id ]
        | SetIngredientName (id, name) ->
            let ingredient = model.Ingredients |> List.find (fun x -> x.Id = id)
            let updatedIngredient = { ingredient with Name = name }
            let updatedIngredients = model.Ingredients |> List.map (fun x -> if x.Id = id then updatedIngredient else x)
            { model with Ingredients = updatedIngredients }, []
        | SetIngredientPrice (id, price) ->
            let ingredient = model.Ingredients |> List.find (fun x -> x.Id = id)
            let updatedIngredient = { ingredient with Price = price }
            let updatedIngredients = model.Ingredients |> List.map (fun x -> if x.Id = id then updatedIngredient else x)
            { model with Ingredients = updatedIngredients }, []
        | IngredientSaved ->
            model, []
        | IngredientDeleted ->
            model, []
            
        | AddIngredientToDetailed ingredient ->
            let newDetails =
                let alreadyAdded = model.DetailedIngredients |> List.exists (fun x -> x.IngredientId = ingredient.Id)
                match alreadyAdded with
                | true ->
                    model.DetailedIngredients
                | false ->
                    let newDetailed = { DetailedIngredient.New ingredient.Id () with Price = ingredient.Price }
                    newDetailed::model.DetailedIngredients
            { model with DetailedIngredients = newDetails }, [  ]
        | DeleteDetailedIngredient id ->
            let newDetailedIngredients =
                model.DetailedIngredients |> List.filter (fun x -> not (x.Id = id))
            { model with DetailedIngredients = newDetailedIngredients }, []
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
                                    backgroundColor = match model.CurrentPage with IngredientsPage -> Color.LightBlue | _ -> Color.Transparent
                                )
                                View.Button(
                                    text = "Fracionar",
                                    command = (fun () -> NavigateTo FractionPage |> dispatch),
                                    backgroundColor = match model.CurrentPage with FractionPage -> Color.LightBlue | _ -> Color.Transparent
                                )
                            ]
                    )
                )
            ),
            detail = View.NavigationPage(
                padding = Thickness(4., 0.),
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
                                        View.Grid(
                                            rowdefs = Auto::[ for _ in model.Ingredients -> Auto ],
                                            coldefs = [ Stars 0.1; Stars 0.7; Stars 0.2 ],
                                            children = [
                                                View.Label(
                                                    text = "Nome",
                                                    verticalTextAlignment = TextAlignment.Center
                                                ).Row(0).Column(1)
                                                View.Label(
                                                    text = "Preço",
                                                    verticalTextAlignment = TextAlignment.Center
                                                ).Row(0).Column(2)
                                                for i in model.Ingredients do
                                                    let row = (model.Ingredients |> List.findIndex (fun x -> x.Equals i)) + 1
                                                    View.Button(
                                                        text = "X",
                                                        backgroundColor = Color.Red,
                                                        textColor = Color.White,
                                                        fontAttributes = FontAttributes.Bold,
                                                        cornerRadius = 40,
                                                        verticalOptions = LayoutOptions.Center,
                                                        horizontalOptions = LayoutOptions.Center,
                                                        height = 40.,
                                                        width = 40.,
                                                        command = (fun () -> DeleteIngredient i |> dispatch)
                                                    ).Row(row).Column(0)
                                                    View.Entry(
                                                        text = i.Name,
                                                        placeholder = "Nome",
                                                        textChanged = (fun args -> SetIngredientName (i.Id, args.NewTextValue) |> dispatch),
                                                        unfocused = fun (args: FocusEventArgs) ->
                                                            match args.IsFocused with
                                                            | true -> () |> ignore
                                                            | false -> SaveIngredient i.Id |> dispatch
                                                    ).Row(row).Column(1)
                                                    View.Entry(
                                                        text = string i.Price,
                                                        keyboard = Keyboard.Numeric,
                                                        textChanged = (fun args -> 
                                                            match Double.TryParse(args.NewTextValue) with
                                                            | false, _ ->
                                                                ()
                                                            | true, parsed ->
                                                                SetIngredientPrice (i.Id, parsed) |> dispatch),
                                                        unfocused = fun (args: FocusEventArgs) ->
                                                            match args.IsFocused with
                                                            | true -> () |> ignore
                                                            | false -> SaveIngredient i.Id |> dispatch
                                                    ).Row(row).Column(2)
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
                                    rowdefs = [ Absolute 30.; Absolute 20.; Star; Absolute 50. ],
                                    coldefs = [ Star ],
                                    children = [
                                        View.CollectionView(
                                            itemsLayout = LinearItemsLayout(ItemsLayoutOrientation.Horizontal, ItemSpacing = 7.),
                                            selectionMode = SelectionMode.Single,
                                            items = [
                                                let availableIngredients = model.Ingredients |> List.filter (fun x -> not <| String.IsNullOrEmpty(x.Name))
                                                for i in availableIngredients do
                                                    View.Label(
                                                        text = i.Name,
                                                        padding = Thickness(2.),
                                                        backgroundColor = Color.LightGreen,
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
                                        ).Row(0)
                                        View.Slider(
                                            value = float model.FractionedBy,
                                            minimumMaximum = (1., 50.),
                                            valueChanged = (fun args -> SetFractionedBy (int (args.NewValue + 0.5)) |> dispatch)
                                        ).Row(1)
                                        let detailedRows = [ Auto ]@[ for _ in 0..model.DetailedIngredients.Length * 2 -> Auto ]
                                        let detailedCols = [ Stars 0.1; Stars 0.2; Stars 0.2; Stars 0.2; Stars 0.2 ]
                                        View.Grid(
                                            rowdefs = detailedRows,
                                            coldefs = detailedCols,
                                            children = [
                                                View.Label(
                                                    text = "Ingrediente",
                                                    verticalTextAlignment = TextAlignment.Center
                                                ).Column(1).Row(0)
                                                View.Label(
                                                    text = "Receita",
                                                    verticalTextAlignment = TextAlignment.Center
                                                ).Column(3).Row(0)
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
                                                ).Column(4).Row(0)
                                                let mutable curRow = 1
                                                for i in model.DetailedIngredients do
                                                    let ingredient = model.Ingredients |> List.find (fun x -> x.Id = i.IngredientId)
                                                    let index = model.DetailedIngredients |> List.findIndex (fun x -> x.Id = i.Id)
                                                    let backgroundColor = match index % 2 = 0 with | false -> Color.White | true -> Color.FromHex("#F3F3F3")
                                                    View.BoxView(
                                                        color = backgroundColor
                                                    ).Row(curRow).ColumnSpan(detailedCols.Length).RowSpan(2)
                                                    View.Button(
                                                        text = "X",
                                                        backgroundColor = Color.Red,
                                                        textColor = Color.White,
                                                        fontAttributes = FontAttributes.Bold,
                                                        cornerRadius = 40,
                                                        verticalOptions = LayoutOptions.Center,
                                                        horizontalOptions = LayoutOptions.Center,
                                                        height = 40.,
                                                        width = 40.,
                                                        command = (fun () -> DeleteDetailedIngredient i.Id |> dispatch)
                                                    ).Column(0).Row(curRow).RowSpan(2)
                                                    View.Label(
                                                        text = ingredient.Name,
                                                        verticalTextAlignment = TextAlignment.Center
                                                    ).Column(1).Row(curRow).RowSpan(2)
                                                    View.Label(
                                                        text = "Medida:",
                                                        verticalTextAlignment = TextAlignment.Center
                                                    ).Column(2).Row(curRow)
                                                    View.Label(
                                                        text = "Preço:",
                                                        verticalTextAlignment = TextAlignment.Center
                                                    ).Column(2).Row(curRow + 1)
                                                    View.Entry(
                                                        text = string i.Measure,
                                                        keyboard = Keyboard.Numeric,
                                                        textChanged = (fun args ->
                                                            match Double.TryParse(args.NewTextValue) with
                                                            | false, _ ->
                                                                ()
                                                            | true, parsed ->
                                                                SetMeasure (i.Id, parsed) |> dispatch)
                                                    ).Column(3).Row(curRow)
                                                    View.Entry(
                                                        text = string i.Price,
                                                        keyboard = Keyboard.Numeric,
                                                        textChanged = (fun args -> 
                                                            match Double.TryParse(args.NewTextValue) with
                                                            | false, _ ->
                                                                ()
                                                            | true, parsed ->
                                                                SetPrice (i.Id, parsed) |> dispatch)
                                                    ).Column(3).Row(curRow + 1)
                                                    View.Label(
                                                        text = sprintf "%.3f" (i.Measure / fraction),
                                                        verticalTextAlignment = TextAlignment.Center,
                                                        horizontalOptions = LayoutOptions.End
                                                    ).Column(4).Row(curRow)
                                                    View.Label(
                                                        text = sprintf "%.3f" (i.Price / fraction),
                                                        verticalTextAlignment = TextAlignment.Center,
                                                        horizontalOptions = LayoutOptions.End
                                                    ).Column(4).Row(curRow + 1)
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
