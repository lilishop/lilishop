package cn.lili.modules.purchase.service;


import cn.lili.modules.purchase.entity.dos.PurchaseOrderItem;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 采购单子内容业务层
 *
 * @author Bulbasaur
 * @since 2020/11/26 16:12
 */
public interface PurchaseOrderItemService extends IService<PurchaseOrderItem> {

    /**
     * 添加采购单子内容
     *
     * @param purchaseOrderId       采购单ID
     * @param purchaseOrderItemList 采购单子内容列表
     * @return 操作结果
     */
    boolean addPurchaseOrderItem(String purchaseOrderId, List<PurchaseOrderItem> purchaseOrderItemList);

}