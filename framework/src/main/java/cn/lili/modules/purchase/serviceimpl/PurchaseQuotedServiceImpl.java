package cn.lili.modules.purchase.serviceimpl;

import cn.lili.common.utils.BeanUtil;
import cn.lili.modules.purchase.entity.dos.PurchaseQuoted;
import cn.lili.modules.purchase.entity.vos.PurchaseQuotedVO;
import cn.lili.modules.purchase.mapper.PurchaseQuotedMapper;
import cn.lili.modules.purchase.service.PurchaseQuotedItemService;
import cn.lili.modules.purchase.service.PurchaseQuotedService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.core.toolkit.Wrappers;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;


/**
 * 采购单报价业务层实现
 *
 * @author Bulbasaur
 * @since 2020/11/26 16:13
 */
@Service
public class PurchaseQuotedServiceImpl extends ServiceImpl<PurchaseQuotedMapper, PurchaseQuoted> implements PurchaseQuotedService {
    @Autowired
    private PurchaseQuotedItemService purchaseQuotedItemService;
    @Override
    @Transactional(rollbackFor = Exception.class)
    public PurchaseQuotedVO addPurchaseQuoted(PurchaseQuotedVO purchaseQuotedVO) {


        PurchaseQuoted purchaseQuoted = new PurchaseQuoted();
        BeanUtil.copyProperties(purchaseQuotedVO, purchaseQuoted);
        //添加报价单
        this.save(purchaseQuoted);
        //添加采购单子内容
        purchaseQuotedItemService.addPurchaseQuotedItem(purchaseQuoted.getId(), purchaseQuotedVO.getPurchaseQuotedItems());
        return purchaseQuotedVO;
    }

    @Override
    public List<PurchaseQuoted> getByPurchaseOrderId(String purchaseOrderId) {
        LambdaQueryWrapper<PurchaseQuoted> lambdaQueryWrapper = Wrappers.lambdaQuery();
        lambdaQueryWrapper.eq(PurchaseQuoted::getPurchaseOrderId, purchaseOrderId);
        lambdaQueryWrapper.orderByDesc(PurchaseQuoted::getCreateTime);
        return this.list(lambdaQueryWrapper);
    }

    @Override
    public PurchaseQuotedVO getById(String id) {
        //获取报价单
        PurchaseQuotedVO purchaseQuotedVO = new PurchaseQuotedVO();
        PurchaseQuoted purchaseQuoted=this.baseMapper.selectById(id);
        BeanUtil.copyProperties(purchaseQuoted, purchaseQuotedVO);
        //获取报价单子内容
        purchaseQuotedVO.setPurchaseQuotedItems(purchaseQuotedItemService.purchaseQuotedItemList(id));
        return purchaseQuotedVO;
    }
}