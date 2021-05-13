package cn.lili.modules.distribution.entity.dto;

import cn.lili.common.security.context.UserContext;
import cn.lili.common.utils.StringUtils;
import cn.lili.common.vo.PageVO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 分销员商品查询条件
 *
 * @author pikachu
 * @date 2020-03-14 23:04:56
 */
@Data
public class DistributionGoodsSearchParams extends PageVO {

    @ApiModelProperty(value = "商品名称")
    private String goodsName;

    @ApiModelProperty(value = "是否已选择")
    private boolean isChecked;


    public <T> QueryWrapper<T> storeQueryWrapper() {
        QueryWrapper<T> queryWrapper = this.distributionQueryWrapper();
        queryWrapper.eq("dg.store_id", UserContext.getCurrentUser().getStoreId());
        return queryWrapper;
    }

    public <T> QueryWrapper<T> distributionQueryWrapper() {
        QueryWrapper<T> queryWrapper = new QueryWrapper<>();
        queryWrapper.eq(StringUtils.isNotEmpty(goodsName), "dg.goods_name", goodsName);
        return queryWrapper;
    }

}
