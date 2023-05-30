package cn.lili.modules.promotion.entity.dto.search;


import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.security.context.UserContext;
import cn.lili.common.security.enums.UserEnums;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

import java.io.Serializable;

/**
 * 砍价活动商品查询通用类
 *
 * @author qiuqiu
 * @date 2020/8/21
 **/
@EqualsAndHashCode(callSuper = true)
@Data
public class KanjiaActivityGoodsParams extends BasePromotionsSearchParams implements Serializable {

    private static final long serialVersionUID = 1344104067705714289L;

    @ApiModelProperty(value = "活动商品")
    private String goodsName;

    @ApiModelProperty(value = "skuId")
    private String skuId;

    @Override
    public <T> QueryWrapper<T> queryWrapper() {
        QueryWrapper<T> queryWrapper = super.queryWrapper();

        if (CharSequenceUtil.isNotEmpty(goodsName)) {
            queryWrapper.like("goods_name", goodsName);
        }
        if (UserContext.getCurrentUser() != null && UserContext.getCurrentUser().getRole().equals(UserEnums.MEMBER)) {
            queryWrapper.gt("stock", 0);
        }
        queryWrapper.eq("delete_flag", false);
        return queryWrapper;
    }

}
