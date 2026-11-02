package cn.lili.modules.promotion.entity.dos;

import cn.lili.mybatis.BaseEntity;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.EqualsAndHashCode;


/**
 * 砍价活动商品实体类
 *
 * @author qiuqiu
 * @date 2020-7-1 10:44 上午
 */
@Data
@TableName("li_kanjia_activity_log")
@Schema(description = "砍价活动日志")
@EqualsAndHashCode(callSuper = false)
public class KanjiaActivityLog extends BaseEntity {


    private static final long serialVersionUID = 3977352717995562783L;

    @Schema(description = "砍价活动参与记录id")
    private String kanjiaActivityId;

    @Schema(description = "砍价客户id")
    private String kanjiaMemberId;

    @Schema(description = "砍价客户名称")
    private String kanjiaMemberName;

    @Schema(description = "砍价客户头像")
    private String kanjiaMemberFace;

    @Schema(description = "砍价金额")
    private Double kanjiaPrice;

    @Schema(description = "剩余购买金额")
    private Double surplusPrice;


}