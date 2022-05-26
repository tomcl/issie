[sheet.fs](https://github.com/tomcl/issie/blob/review-tc/src/Renderer/DrawBlock/Sheet.fs) Lines 561-823

Context: 

1. This code took me a _long_ time to write because the core Snap types were difficult to get right. Once I had those the code was quite quick.
2. The code uses a lot of functional abstraction - only possible because of the correct types and helpers.
3. When a symbol or segment is dragged, at the start of the drag, the set of possible snap positions is calculated once - this static data is used during the drag to implement the snap operation.
4. The same code is used for different purposes to implement symbol or segment snaps.
5. To understand code, download Issie, add a few components to a project, see how snapping works dotted red line guides).
