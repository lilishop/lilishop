package cn.lili.modules.order.trade.entity.dos;

import cn.lili.common.security.enums.UserEnums;
import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.format.annotation.DateTimeFormat;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import java.util.Date;

/**
 * 售后日志
 *
 * @author Bulbasaur
 * @date 2020-03-25 2:30 下午
 */
@Data
@Entity
@Table(name = "li_after_sale_log")
@TableName("li_after_sale_log")
@ApiModel(value = "售后日志")
@NoArgsConstructor
public class AfterSaleLog {

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

    @ApiModelProperty(value = "售后服务单号")
    private String sn;

    @ApiModelProperty(value = "操作者id(可以是卖家)")
    private String operatorId;

    /**
     * @see UserEnums
     */
    @ApiModelProperty(value = "操作者类型")
    private String operatorType;


    @ApiModelProperty(value = "操作者名称")
    private String operatorName;

    @ApiModelProperty(value = "日志信息")
    private String message;

    public AfterSaleLog(String sn, String operatorId, String operatorType, String operatorName, String message) {
        this.sn = sn;
        this.operatorId = operatorId;
        this.operatorType = operatorType;
        this.operatorName = operatorName;
        this.message = message;
    }
}
