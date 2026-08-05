package cn.lili.modules.distribution.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.util.List;

/**
 * 分销招募申请DTO
 *
 * @author pikachu
 */
@Data
public class DistributionRecruitApplyDTO {

    @Schema(description = "招募邀请令牌")
    private String recruitToken;

    @Schema(description = "是否已同意分销推广服务协议")
    private Boolean agreementAccepted;

    @Schema(description = "动态填写内容（后台配置什么字段，用户就填什么）")
    private List<FormItem> formItems;

    @Data
    public static class FormItem {
        @Schema(description = "字段ID")
        private String fieldId;
        @Schema(description = "填写内容")
        private String fieldValue;
    }
}
