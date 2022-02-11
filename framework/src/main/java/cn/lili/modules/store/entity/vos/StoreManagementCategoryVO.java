package cn.lili.modules.store.entity.vos;

import cn.lili.common.utils.BeanUtil;
import cn.lili.modules.goods.entity.dos.Category;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * 店铺经营范围
 * @author Bulbasaur
 * @since 2020/12/11 16:18
 */
@Data
@NoArgsConstructor
public class StoreManagementCategoryVO extends Category {

    @ApiModelProperty(value = "已选择")
    private Boolean selected;

    public StoreManagementCategoryVO(Category category) {
        BeanUtil.copyProperties(this, category);
    }

}
