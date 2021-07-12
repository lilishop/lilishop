package cn.lili.modules.purchase.serviceimpl;

import cn.lili.modules.purchase.entity.dos.PurchaseQuotedItem;
import cn.lili.modules.purchase.mapper.PurchaseQuotedItemMapper;
import cn.lili.modules.purchase.service.PurchaseQuotedItemService;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;


/**
 * 采购单子内容业务层实现
 *
 * @author Bulbasaur
 * @date 2020/11/26 16:13
 */
@Service
public class PurchaseQuotedItemServiceImpl extends ServiceImpl<PurchaseQuotedItemMapper, PurchaseQuotedItem> implements PurchaseQuotedItemService {

    @Override
    public boolean addPurchaseQuotedItem(String purchaseQuotedId, List<PurchaseQuotedItem> purchaseQuotedItemList) {
        for (PurchaseQuotedItem purchaseQuotedItem : purchaseQuotedItemList) {
            purchaseQuotedItem.setPurchaseQuotedId(purchaseQuotedId);
            this.save(purchaseQuotedItem);
        }
        return true;
    }

    @Override
    public List<PurchaseQuotedItem> purchaseQuotedItemList(String purchaseQuotedId) {
        return null;
    }
}