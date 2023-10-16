package cn.lili.modules.im.entity.dto;

import cn.hutool.core.text.CharSequenceUtil;
import cn.lili.common.vo.PageVO;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * @author chc
 * @since 2022/6/2114:46
 */
@EqualsAndHashCode(callSuper = true)
@Data
@ApiModel
public class ImQueryParams extends PageVO {

    private static final long serialVersionUID = 5792718094087541134L;

    @ApiModelProperty("用户Id")
    private String memberId;

    @ApiModelProperty("店铺Id")
    private String storeId;

    public <T> QueryWrapper<T> queryWrapper() {
        QueryWrapper<T> queryWrapper = new QueryWrapper<>();
        if (CharSequenceUtil.isNotEmpty(memberId)) {
            queryWrapper.eq("member_id", memberId);
        }
        if (CharSequenceUtil.isNotEmpty(storeId)) {
            queryWrapper.eq("store_id", storeId);
        }
        queryWrapper.eq("delete_flag", false);
        queryWrapper.orderByDesc("create_time");
        return queryWrapper;
    }
}
