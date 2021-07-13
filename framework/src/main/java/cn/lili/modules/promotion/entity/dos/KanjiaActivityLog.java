package cn.lili.modules.promotion.entity.dos;

import cn.lili.base.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import javax.persistence.Entity;
import javax.persistence.Table;

/**
 * 砍价活动商品实体类
 *
 * @author qiuqiu
 * @date 2020-7-1 10:44 上午
 */
@Data
@Entity
@Table(name = "li_kanjia_activity_log")
@TableName("li_kanjia_activity_log")
@ApiModel(value = "砍价活动日志对象")
public class KanjiaActivityLog extends BaseEntity {


    private static final long serialVersionUID = 3977352717995562783L;

    @ApiModelProperty(value = "砍价活动参与记录id")
    private String kanjiaActivityId;

    @ApiModelProperty(value = "砍价会员id")
    private String kanjiaMemberId;

    @ApiModelProperty(value = "砍价会员名称")
    private String kanjiaMemberName;

    @ApiModelProperty(value = "砍价会员头像")
    private String kanjiaMemberFace;

    @ApiModelProperty(value = "砍价金额")
    private Double kanjiaPrice;

    @ApiModelProperty(value = "剩余购买金额")
    private Double surplusPrice;


}