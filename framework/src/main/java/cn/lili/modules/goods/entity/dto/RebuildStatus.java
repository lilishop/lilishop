package cn.lili.modules.goods.entity.dto;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * 商品参数倒排全量重建进度（存 Redis，跨实例一致）。
 *
 * @author Mike
 * @date 2026-09-03
 */
@Data
@Schema(description = "商品参数倒排全量重建进度")
public class RebuildStatus {

    @Schema(description = "是否正在运行")
    private Boolean running = false;

    @Schema(description = "已扫描商品数")
    private Integer scanned = 0;

    @Schema(description = "已成功重建商品数")
    private Integer rebuilt = 0;

    @Schema(description = "重建失败的商品ID")
    private List<String> failedGoodsIds = new ArrayList<>();

    @Schema(description = "开始时间")
    private Date startedAt;

    @Schema(description = "结束时间")
    private Date finishedAt;

    @Schema(description = "状态说明")
    private String message;
}
