package cn.lili.modules.finance.export;

import cn.lili.common.exception.ServiceException;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

/**
 * {@link FinanceExportHelper} 导出行数上限校验测试。
 */
class FinanceExportHelperTest {

    @Test
    void checkRowLimit_withinLimit() {
        assertDoesNotThrow(() -> FinanceExportHelper.checkRowLimit(FinanceExportHelper.MAX_EXPORT_ROWS));
    }

    @Test
    void checkRowLimit_exceedsLimit() {
        assertThrows(ServiceException.class,
                () -> FinanceExportHelper.checkRowLimit(FinanceExportHelper.MAX_EXPORT_ROWS + 1));
    }
}
