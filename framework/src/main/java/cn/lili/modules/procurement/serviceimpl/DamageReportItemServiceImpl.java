package cn.lili.modules.procurement.serviceimpl;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.goods.entity.dos.GoodsSku;
import cn.lili.modules.goods.service.GoodsSkuService;
import cn.lili.modules.procurement.entity.dos.DamageReportItem;
import cn.lili.modules.procurement.mapper.DamageReportItemMapper;
import cn.lili.modules.procurement.service.DamageReportItemService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.List;

/**
 * 报损单明细业务实现
 * 提供按报损单ID查询明细
 * @author Bulbasaur
 * @since 2025-12-18
 */
@Service
public class DamageReportItemServiceImpl extends ServiceImpl<DamageReportItemMapper, DamageReportItem> implements DamageReportItemService {

    @Autowired
    private GoodsSkuService goodsSkuService;

    @Override
    public List<DamageReportItem> listByReportId(String reportId) {
        LambdaQueryWrapper<DamageReportItem> wrapper = Wrappers.lambdaQuery();
        wrapper.eq(DamageReportItem::getReportId, reportId);
        List<DamageReportItem> items = this.list(wrapper);
        items.forEach(this::fillGoodsNameIfMissing);
        return items;
    }

    private void fillGoodsNameIfMissing(DamageReportItem item) {
        if (CharSequenceUtil.isNotEmpty(item.getGoodsName()) || CharSequenceUtil.isEmpty(item.getSkuId())) {
            return;
        }
        GoodsSku sku = goodsSkuService.getGoodsSkuByIdFromCache(item.getSkuId());
        if (sku != null) {
            item.setGoodsName(sku.getGoodsName());
        }
    }
}
