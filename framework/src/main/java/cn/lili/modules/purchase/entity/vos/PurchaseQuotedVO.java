package cn.lili.modules.purchase.entity.vos;

import cn.lili.modules.purchase.entity.dos.PurchaseQuoted;
import cn.lili.modules.purchase.entity.dos.PurchaseQuotedItem;
import lombok.Data;

import java.util.List;

/**
 * 报价单VO
 *
 * @author Bulbasaur
 * @since 2020/11/26 19:54
 */
@Data
public class PurchaseQuotedVO extends PurchaseQuoted {

    private List<PurchaseQuotedItem> purchaseQuotedItems;
}
