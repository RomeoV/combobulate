# -*- combobulate-test-point-overlays: ((1 outline 135) (2 outline 156) (3 outline 166)); eval: (combobulate-test-fixture-mode t); -*-
function outer()
    x = 1
    function inner()
        y = 2
    end
    return x
end
