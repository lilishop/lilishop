package cn.lili.modules.distribution.entity.dos;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

/**
 * 分销员已选择分销商品
 *
 * @author pikachu
 * @date 2020-03-14 23:04:56
 */
@Data
@Entity
@ApiModel(value = "分销商已选择分销商品")
@Table(name = "li_distribution_selected_goods")
@TableName("li_distribution_selected_goods")
@NoArgsConstructor
public class DistributionSelectedGoods {


    @Id
    @TableId
    @TableField
    @Column(columnDefinition = "bigint(20)")
    @ApiModelProperty(value = "唯一标识", hidden = true)
    private String id;

    @ApiModelProperty(value = "分销员ID")
    private String distributionId;

    @ApiModelProperty(value = "分销商品ID")
    private String distributionGoodsId;

    public DistributionSelectedGoods(String distributionId, String distributionGoodsId) {
        this.distributionId = distributionId;
        this.distributionGoodsId = distributionGoodsId;
    }
}
