package cn.lili.modules.member.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;


/**
 * 浏览历史
 *
 * @author Chopper
 * @since 2020/11/17 7:22 下午
 */
@Data
@TableName("li_foot_print")
@ApiModel(value = "浏览历史")
@NoArgsConstructor
@AllArgsConstructor
public class FootPrint extends BaseEntity {

    private static final long serialVersionUID = 1L;


    @ApiModelProperty(value = "会员ID")
    private String memberId;

    @ApiModelProperty(value = "商品ID")
    private String goodsId;

    @ApiModelProperty(value = "规格ID")
    private String skuId;

}