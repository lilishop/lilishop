/**
 * 财务对账与报表模块。
 * <p>
 * 职责划分：
 * <ul>
 *   <li>{@link cn.lili.modules.finance.service.FinanceExportService} — 各类流水、结算单的 Excel/ZIP 导出</li>
 *   <li>{@link cn.lili.modules.finance.service.FinanceReportService} — 平台/店铺维度财务报表查询与导出</li>
 *   <li>{@link cn.lili.modules.finance.export.FinanceExportHelper} — 导出公共能力（行数上限、xlsx 写出）</li>
 * </ul>
 * 金额口径与 {@link cn.lili.modules.store.entity.dos.Bill} 及 {@code li_store_flow.bill_price} 保持一致，
 * 详见 {@code docs/finance/口径说明.md}。
 */
package cn.lili.modules.finance;
