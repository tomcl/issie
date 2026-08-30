/// What each parameterisable slot of a component sets, and what may go in it.
///
/// ComponentSlots is the one place that knows both - trySetSlotValue writes a value into a
/// component, constraintsFor says what values are allowed - and the two are here together because
/// they are two halves of one fact. They were once built inline at each properties box, which is
/// how a bound came to be frozen at the width the box happened to be showing.
///
/// The memory slots have tests of their own in MemoryParameters, where the contents question they
/// raise belongs.
module ComponentSlotTests

open Expecto
open CommonTypes
open ParameterTypes

let tests =
    testList "ComponentSlots" [

        // --- the value of a constant, and what a bus comparator compares against ---
        //
        // The component's SECOND number, in the IO slot for the reason BusSelection's LSB is. Until
        // these existed the value was the one number on a parameterised constant that had to be
        // fixed, while its width could follow a property.

        test "a constant's value is a slot, and setting it moves the drawn text with it" {
            let set v = ComponentSlots.trySetSlotValue (IO "C1") v (Constant1 (8, 0I, "0"))
            Expect.equal (set 31I) (Some (Constant1 (8, 31I, "31")))
                "the value is set, and the text - which is what the symbol draws - says what it is"
            Expect.equal (set -1I) (Some (Constant1 (8, -1I, "-1")))
                "a constant may be negative"
        }

        test "a bus comparator's value is a slot on the current component type" {
            Expect.equal
                (ComponentSlots.trySetSlotValue (IO "EQ1") 255I (BusCompare1 (8, 0I, "0")))
                (Some (BusCompare1 (8, 255I, "255")))
                "BusCompare1 is the type Issie creates; the legacy BusCompare had this already"
        }

        test "both values are bounded by their own component's width" {
            // signed or unsigned, which is the range NumberHelpers.checkWidth accepts and so what
            // the text boxes these replaced accepted
            let bounds slot comp = ComponentSlots.constraintsFor slot comp
            Expect.equal
                (bounds (IO "C1") (Constant1 (4, 0I, "0")))
                [ MinVal (PInt -8I, "Constant value must be at least -8 in 4 bits")
                  MaxVal (PInt 15I, "Constant value must fit in 4 bits") ]
                "a 4-bit constant holds -8..15"
            Expect.equal
                (bounds (IO "EQ1") (BusCompare1 (4, 0I, "0")))
                [ MinVal (PInt -8I, "Comparison value must be at least -8 in 4 bits")
                  MaxVal (PInt 15I, "Comparison value must fit in 4 bits") ]
                "and so does what a 4-bit comparator compares against"
        }

        test "the width of these two is still its own slot" {
            // Buswidth and IO are different slots on the same component: the value must not be able
            // to set the width, which is what sharing one would have meant.
            Expect.equal
                (ComponentSlots.trySetSlotValue Buswidth 16I (Constant1 (8, 3I, "3")))
                (Some (Constant1 (16, 3I, "3")))
                "the width changes and the value is left alone"
            Expect.equal
                (ComponentSlots.trySetSlotValue Buswidth 16I (BusCompare1 (8, 3I, "3")))
                (Some (BusCompare1 (16, 3I, "3")))
                "the same for the comparator"
        }
    ]
