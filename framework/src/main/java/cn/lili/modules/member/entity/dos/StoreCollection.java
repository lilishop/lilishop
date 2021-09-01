package cn.lili.modules.member.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;


/**
 * 会员店铺收藏
 *
 * @author Chopper
 * @since 2020/11/18 3:32 下午
 */
@Data
@TableName("li_store_collection")
@ApiModel(value = "会员收藏")
@NoArgsConstructor
@AllArgsConstructor
public class StoreCollection extends BaseEntity {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "会员id")
    private String memberId;

    @ApiModelProperty(value = "店铺id")
    private String storeId;


}