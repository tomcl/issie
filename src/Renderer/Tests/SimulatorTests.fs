module SimulatorTests

open DiagramTypes
open CanvasStates

let testCasesSimulator = [
    "Simple circuit with one input and one output (zero)",
    (state3, [ComponentId "input-node0", Zero]),
    [(ComponentId "output-node0", ComponentLabel "output"), Zero]

    "Simple circuit with one input and one output (one)",
    (state3, [ComponentId "input-node0", One]),
    [(ComponentId "output-node0", ComponentLabel "output"), One]

    "Simple circuit with one input connected to two outputs (Zero)",
    (state4, [ComponentId "input-node0", Zero]),
    [
        (ComponentId "output-node0", ComponentLabel "output"), Zero
        (ComponentId "output-node1", ComponentLabel "output"), Zero
    ]

    "Simple circuit with one input connected to two outputs (One)",
    (state4, [ComponentId "input-node0", One]),
    [
        (ComponentId "output-node0", ComponentLabel "output"), One
        (ComponentId "output-node1", ComponentLabel "output"), One
    ]

    "Two inputs; one And; one output (Zero, Zero)",
    (state6, [ComponentId "top-input", Zero; ComponentId "bottom-input", Zero]),
    [(ComponentId "output", ComponentLabel ""), Zero]

    "Two inputs; one And; one output (Zero, One)",
    (state6, [ComponentId "top-input", Zero; ComponentId "bottom-input", One]),
    [(ComponentId "output", ComponentLabel ""), Zero]

    "Two inputs; one And; one output (One, Zero)",
    (state6, [ComponentId "top-input", One; ComponentId "bottom-input", Zero]),
    [(ComponentId "output", ComponentLabel ""), Zero]

    "Two inputs; one And; one output (One, One)",
    (state6, [ComponentId "top-input", One; ComponentId "bottom-input", One]),
    [(ComponentId "output", ComponentLabel ""), One]

    "One bit adder (Zero, Zero)",
    (state13, [
        ComponentId "2953603d-44e4-5c1f-3fb1-698f7863b6b5", Zero;
        ComponentId "170e69f4-b3d7-d9e0-9f1d-6a564ba62062", Zero;
    ]),
    [
        (ComponentId "9aaf18a9-b3ac-bf51-1ed3-625baa1ff6a9", ComponentLabel "Sum"), Zero
        (ComponentId "94da6dd7-a263-a3ec-ec76-bfa07b0b0f34", ComponentLabel "Carry"), Zero
    ]

    "One bit adder (Zero, One)",
    (state13, [
        ComponentId "2953603d-44e4-5c1f-3fb1-698f7863b6b5", Zero;
        ComponentId "170e69f4-b3d7-d9e0-9f1d-6a564ba62062", One;
    ]),
    [
        (ComponentId "9aaf18a9-b3ac-bf51-1ed3-625baa1ff6a9", ComponentLabel "Sum"), One
        (ComponentId "94da6dd7-a263-a3ec-ec76-bfa07b0b0f34", ComponentLabel "Carry"), Zero
    ]

    "One bit adder (One, Zero)",
    (state13, [
        ComponentId "2953603d-44e4-5c1f-3fb1-698f7863b6b5", One;
        ComponentId "170e69f4-b3d7-d9e0-9f1d-6a564ba62062", Zero;
    ]),
    [
        (ComponentId "9aaf18a9-b3ac-bf51-1ed3-625baa1ff6a9", ComponentLabel "Sum"), One
        (ComponentId "94da6dd7-a263-a3ec-ec76-bfa07b0b0f34", ComponentLabel "Carry"), Zero
    ]

    "One bit adder (One, One)",
    (state13, [
        ComponentId "2953603d-44e4-5c1f-3fb1-698f7863b6b5", One;
        ComponentId "170e69f4-b3d7-d9e0-9f1d-6a564ba62062", One;
    ]),
    [
        (ComponentId "9aaf18a9-b3ac-bf51-1ed3-625baa1ff6a9", ComponentLabel "Sum"), Zero
        (ComponentId "94da6dd7-a263-a3ec-ec76-bfa07b0b0f34", ComponentLabel "Carry"), One
    ]
]