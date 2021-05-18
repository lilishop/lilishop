package cn.lili.modules.broadcast.entity.dos;

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
 * 直播商品
 *
 * @author Bulbasaur
 * @date: 2021/5/18 5:42 下午
 */
@Data
@Entity
@ApiModel(value = "直播商品")
@TableName("li_studio_commodity")
@Table(name = "li_studio_commodity")
@NoArgsConstructor
public class StudioCommodity {

    @Id
    @TableId
    @TableField
    @Column(columnDefinition = "bigint(20)")
    @ApiModelProperty(value = "唯一标识", hidden = true)
    private String id;

    @ApiModelProperty(value = "房间ID")
    private Integer roomId;

    @ApiModelProperty(value = "商品ID")
    private Integer goodsId;

    public StudioCommodity(Integer roomId, Integer goodsId) {
        this.roomId = roomId;
        this.goodsId = goodsId;
    }
}
