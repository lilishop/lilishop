package cn.lili.modules.goods.entity.dto;

import cn.lili.modules.goods.entity.dos.Wholesale;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author paulG
 * @since 2022/5/25
 **/
@Data
@EqualsAndHashCode(callSuper = true)
public class WholesaleDTO extends Wholesale {
    private static final long serialVersionUID = 853297561151783335L;

}
