package cn.lili.modules.member.entity.dos;


import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.format.annotation.DateTimeFormat;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import java.util.Date;

/**
 * 会员积分历史
 *
 * @author Bulbasaur
 * @date 2020-02-25 14:10:16
 */
@Data
@Entity
@Table(name = "li_member_points_history")
@TableName("li_member_points_history")
@ApiModel(value = "会员积分历史")
public class MemberPointsHistory {

    private static final long serialVersionUID = 1L;

    @Id
    @TableId
    @TableField
    @Column(columnDefinition = "bigint(20)")
    @ApiModelProperty(value = "唯一标识", hidden = true)
    private String id;

    @CreatedBy
    @TableField(fill = FieldFill.INSERT)
    @ApiModelProperty(value = "创建者", hidden = true)
    private String createBy;

    @CreatedDate
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(fill = FieldFill.INSERT)
    @ApiModelProperty(value = "创建时间", hidden = true)
    private Date createTime;

    @ApiModelProperty(value = "会员ID")
    private String memberId;

    @ApiModelProperty(value = "会员名称")
    private String memberName;

    @ApiModelProperty(value = "当前积分")
    private Long point;

    @ApiModelProperty(value = "消费之前积分")
    private Long beforePoint;

    @ApiModelProperty(value = "消费积分")
    private Long variablePoint;

    @ApiModelProperty(value = "content")
    private String content;

    /**
     * @see cn.lili.modules.member.entity.enums.PointTypeEnum
     */
    @ApiModelProperty(value = "消费积分类型")
    private String pointType;

}