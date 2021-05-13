package cn.lili.modules.statistics.model.dos;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.util.Date;

/**
 * 平台pv统计
 *
 * @author Chopper
 * @date 2020-06-19 17:50
 */
@Data
@Entity
@Table(name = "li_s_platform_view_data")
@TableName("li_s_platform_view_data")
@ApiModel(value = "平台pv统计")
public class PlatformViewData {


    @Id
    @TableId
    @TableField
    @Column(columnDefinition = "bigint(20)")
    @ApiModelProperty(value = "唯一标识", hidden = true)
    private String id;

    @ApiModelProperty(value = "pv数量")
    private Long pvNum;

    @ApiModelProperty(value = "uv数量")
    private Long uvNum;


    @ApiModelProperty(value = "统计日")
    private Date date;

    //默认是平台流量统计//

    @ApiModelProperty(value = "店铺id")
    private String storeId = "-1";
}
