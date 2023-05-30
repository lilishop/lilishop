package cn.lili.modules.page.entity.dos;

import cn.lili.common.security.sensitive.Sensitive;
import cn.lili.common.security.sensitive.enums.SensitiveStrategy;
import cn.lili.modules.page.entity.enums.FeedbackTypeEnum;
import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.EqualsAndHashCode;
import org.hibernate.validator.constraints.Length;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.format.annotation.DateTimeFormat;

import javax.validation.constraints.NotEmpty;
import java.util.Date;

/**
 * 意见反馈
 *
 * @author Bulbasaur
 * @since 2020/12/10 17:42
 */
@EqualsAndHashCode(callSuper = true)
@Data
@TableName("li_feedback")
@ApiModel(value = "意见反馈")
public class Feedback extends BaseIdEntity {

    private static final long serialVersionUID = 1L;

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
    @Length(max = 11, message = "手机号不能超过11位")
    @Sensitive(strategy = SensitiveStrategy.PHONE)
    private String mobile;

    @ApiModelProperty(value = "图片，多个图片使用：(，)分割")
    @Length(max = 255, message = "图片上传太多啦，请选择删除掉")
    private String images;

    /**
     * 类型
     *
     * @see FeedbackTypeEnum
     */
    @ApiModelProperty(value = "类型", allowableValues = "FUNCTION,OPTIMIZE,OTHER")
    private String type;

}