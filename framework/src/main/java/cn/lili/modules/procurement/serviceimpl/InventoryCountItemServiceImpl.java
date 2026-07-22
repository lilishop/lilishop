package cn.lili.modules.procurement.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.finance.export.FinanceExportHelper;
import cn.lili.modules.procurement.entity.dos.InventoryCountItem;
import cn.lili.modules.procurement.mapper.InventoryCountItemMapper;
import cn.lili.modules.procurement.service.InventoryCountItemService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import jakarta.servlet.http.HttpServletResponse;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

/**
 * 盘点单明细业务实现
 * 提供按盘点单ID分页及列表查询
 * @author Bulbasaur
 * @since 2025-12-18
 */
@Service
public class InventoryCountItemServiceImpl extends ServiceImpl<InventoryCountItemMapper, InventoryCountItem> implements InventoryCountItemService {

    @Override
    public IPage<InventoryCountItem> pageByCountId(String countId, Page<InventoryCountItem> page, String goodsName) {
        LambdaQueryWrapper<InventoryCountItem> wrapper = Wrappers.lambdaQuery();
        wrapper.eq(InventoryCountItem::getCountId, countId);
        if (CharSequenceUtil.isNotEmpty(goodsName)) {
            wrapper.like(InventoryCountItem::getGoodsName, goodsName);
        }
        wrapper.orderByDesc(InventoryCountItem::getCreateTime);
        return this.page(page, wrapper);
    }

    @Override
    public List<InventoryCountItem> listByCountId(String countId) {
        LambdaQueryWrapper<InventoryCountItem> wrapper = Wrappers.lambdaQuery();
        wrapper.eq(InventoryCountItem::getCountId, countId);
        return this.list(wrapper);
    }

    @Override
    public void exportExcel(String countId, String sn, HttpServletResponse response) {
        List<InventoryCountItem> items = listByCountId(countId);
        FinanceExportHelper.checkRowLimit(items.size());
        List<String> headers = List.of("商品名称", "规格", "SKU ID", "上架状态", "库存数量");
        List<List<Object>> rows = new ArrayList<>();
        for (InventoryCountItem item : items) {
            List<Object> row = new ArrayList<>();
            row.add(item.getGoodsName() == null ? "" : item.getGoodsName());
            row.add(item.getSkuName() == null ? "" : item.getSkuName());
            row.add(item.getSkuId() == null ? "" : item.getSkuId());
            row.add(marketEnableText(item.getMarketEnable()));
            row.add(item.getQuantity() == null ? 0 : item.getQuantity());
            rows.add(row);
        }
        String fileName = "盘点单_" + (CharSequenceUtil.isNotEmpty(sn) ? sn : countId);
        FinanceExportHelper.writeExcel(response, fileName, writer ->
                FinanceExportHelper.writeSheet(writer, "盘点明细", headers, rows));
    }

    private String marketEnableText(String marketEnable) {
        if ("UPPER".equals(marketEnable)) {
            return "上架";
        }
        if ("DOWN".equals(marketEnable)) {
            return "下架";
        }
        return marketEnable == null ? "" : marketEnable;
    }
}
