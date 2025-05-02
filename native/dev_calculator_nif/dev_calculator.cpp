#include "include/dev_calculator.h"
#include "include/dev_calculator_log.h"

double perform_calculation(const char* operation, const double* operands) {
    std::string op(operation);
	//DRV_PRINT("perform test");
    if (op == "add") {
        return operands[0] + operands[1];
    } else if (op == "subtract") {
        return operands[0] - operands[1];
    } else if (op == "multiply") {
        return operands[0] * operands[1];
    } else if (op == "divide") {
        return operands[0] / operands[1];
    } else {
        return 0.0; // Default case, handle error appropriately
    }
}
