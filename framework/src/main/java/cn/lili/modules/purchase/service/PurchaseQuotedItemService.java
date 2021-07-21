package cn.lili.modules.purchase.service;


import cn.lili.modules.purchase.entity.dos.PurchaseQuotedItem;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 采购单子内容业务层
 *
 * @author Bulbasaur
 * @since 2020/11/26 16:12
 */
public interface PurchaseQuotedItemService extends IService<PurchaseQuotedItem> {

    /**
     * 添加报价单子内容
     *
     * @param PurchaseQuotedId       采购单ID
     * @param purchaseQuotedItemList 采购单子内容列表
     * @return 操作结果
     */
    boolean addPurchaseQuotedItem(String PurchaseQuotedId, List<PurchaseQuotedItem> purchaseQuotedItemList);

    /**
     * 获取报价单子内容列表
     *
     * @param purchaseQuotedId 报价单ID
     * @return 报价单子内容列表
     */
    List<PurchaseQuotedItem> purchaseQuotedItemList(String purchaseQuotedId);

}