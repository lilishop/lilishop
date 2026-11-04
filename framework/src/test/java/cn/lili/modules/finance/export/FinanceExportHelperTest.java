package cn.lili.modules.finance.export;

import cn.lili.common.exception.ServiceException;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * {@link FinanceExportHelper} 导出行数上限校验测试。
 */
class FinanceExportHelperTest {

    /**
     * 验证导出行数未超过限制时允许继续。
     */
    @Test
    void checkRowLimit_withinLimit() {
        assertDoesNotThrow(() -> FinanceExportHelper.checkRowLimit(FinanceExportHelper.MAX_EXPORT_ROWS));
    }

    /**
     * 验证导出行数超过限制时抛出异常。
     */
    @Test
    void checkRowLimit_exceedsLimit() {
        assertThrows(ServiceException.class,
                () -> FinanceExportHelper.checkRowLimit(FinanceExportHelper.MAX_EXPORT_ROWS + 1));
    }
}
