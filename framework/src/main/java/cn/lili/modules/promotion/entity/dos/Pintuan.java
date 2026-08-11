package cn.lili.modules.promotion.entity.dos;

import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import jakarta.validation.constraints.Max;
import jakarta.validation.constraints.Min;
import jakarta.validation.constraints.NotNull;
import lombok.Data;
import lombok.EqualsAndHashCode;

/**
 * 拼团活动实体类
 *
 * @author Chopper
 * @since 2020-03-19 10:44 上午
 */
@Data
@TableName("li_pintuan")
@Schema(description = "拼团")
@EqualsAndHashCode(callSuper = false)
public class Pintuan extends BasePromotions {


    private static final long serialVersionUID = -8465716592648602604L;


    @Min(message = "成团人数需大于等于2", value = 2)
    @Max(message = "成团人数最多10人", value = 10)
    @NotNull(message = "成团人数必填")
    @Schema(description = "成团人数")
    private Integer requiredNum;

    @Min(message = "限购数量必须为数字", value = 0)
    @NotNull(message = "限购数量必填")
    @Schema(description = "限购数量")
    private Integer limitNum;

    @Schema(description = "虚拟成团", requiredMode = Schema.RequiredMode.REQUIRED)
    @NotNull(message = "虚拟成团必填")
    private Boolean fictitious;

    @Schema(description = "拼团规则")
    private String pintuanRule;


}