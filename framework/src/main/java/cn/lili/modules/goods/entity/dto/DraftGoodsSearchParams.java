package cn.lili.modules.goods.entity.dto;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.modules.goods.entity.enums.DraftGoodsSaveType;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

/**
 * 草稿商品搜索对象
 *
 * @author paulG
 * @since 2020/12/21
 **/
@Data
public class DraftGoodsSearchParams extends GoodsSearchParams {

    private static final long serialVersionUID = -1057830772267228050L;

    /**
     * @see DraftGoodsSaveType
     */
    @ApiModelProperty(value = "草稿商品保存类型")
    private String saveType;

    @Override
    public <T> QueryWrapper<T> queryWrapper() {
        QueryWrapper<T> queryWrapper = super.queryWrapper();
        if (CharSequenceUtil.isNotEmpty(saveType)) {
            queryWrapper.eq("save_type", saveType);
        }
        return queryWrapper;
    }
}
