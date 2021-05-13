package cn.lili.modules.statistics.model.dos;

import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.util.Date;

/**
 * 会员统计
 * @author Chopper
 * @date 2020/11/17 7:34 下午
 */
@Data
@Entity
@Table(name = "li_member_statistics_data")
@TableName("li_member_statistics_data")
@ApiModel(value = "会员统计")
public class MemberStatisticsData {

    private static final long serialVersionUID = 1L;

    @Id
    @TableId
    @TableField
    @Column(columnDefinition = "bigint(20)")
    @ApiModelProperty(value = "唯一标识", hidden = true)
    private String id;

    @ApiModelProperty(value = "统计日")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd")
    private Date createDate;

    @ApiModelProperty(value = "当前会员数量")
    private Integer memberCount;

    @ApiModelProperty(value = "新增会员数量")
    private Integer newlyAdded;

    @ApiModelProperty(value = "当日活跃数量")
    private Integer activeQuantity;


}