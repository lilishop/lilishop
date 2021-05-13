package cn.lili.modules.page.entity.dos;

import cn.lili.base.BaseEntity;
import cn.lili.common.utils.SnowFlake;
import cn.lili.modules.page.entity.enums.FeedbackTypeEnum;
import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import org.hibernate.validator.constraints.Length;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.format.annotation.DateTimeFormat;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.validation.constraints.NotEmpty;
import java.io.Serializable;
import java.util.Date;

/**
 * 意见反馈
 *
 * @author Bulbasaur
 * @date 2020/12/10 17:42
 */
@Data
@Entity
@Table(name = "li_feedback")
@TableName("li_feedback")
@ApiModel(value = "意见反馈")
public class Feedback implements Serializable {

    private static final long serialVersionUID = 1L;
    @Id
    @TableId
    @TableField
    @Column(columnDefinition = "bigint(20)")
    @ApiModelProperty(value = "唯一标识", hidden = true)
    private String id;

    @CreatedDate
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @TableField(fill = FieldFill.INSERT)
    @ApiModelProperty(value = "创建时间", hidden = true)
    private Date createTime;

    @ApiModelProperty(value = "会员名称", hidden = true)
    private String userName;

    @ApiModelProperty(value = "反馈内容")
    @NotEmpty(message = "反馈内容不能为空")
    @Length(max = 500, message = "反馈内容不能超过500个字符")
    private String context;

    @ApiModelProperty(value = "手机号")
    private String mobile;

    @ApiModelProperty(value = "图片，多个图片使用：(，)分割")
    private String images;

    /**
     * 类型
     *
     * @see FeedbackTypeEnum
     */
    @ApiModelProperty(value = "类型", allowableValues = "FUNCTION,OPTIMIZE,OTHER")
    private String type;

}