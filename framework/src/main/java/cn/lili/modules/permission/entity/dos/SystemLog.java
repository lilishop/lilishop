package cn.lili.modules.permission.entity.dos;

import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * 系统操作日志
 */
@Data
@TableName("li_system_log")
@ToString
@NoArgsConstructor
@Accessors(chain = true)
public class SystemLog implements Serializable {

    private static final long serialVersionUID = -8995552592401630086L;

    @TableId
    @Schema(description = "id")
    private String id;

    @Schema(description = "日志记录时间")
    private Long createTime;

    @Schema(description = "请求用户")
    private String username;

    @Schema(description = "请求路径")
    private String requestUrl;

    @Schema(description = "请求参数")
    private String requestParam;

    @Schema(description = "响应参数")
    private String responseBody;

    @Schema(description = "ip")
    private String ip;

    @Schema(description = "方法操作名称")
    private String name;

    @Schema(description = "请求类型")
    private String requestType;

    @Schema(description = "自定义日志内容")
    private String customerLog;

    @Schema(description = "ip信息")
    private String ipInfo;

    @Schema(description = "花费时间")
    private Integer costTime;

    @Schema(description = "商家")
    private Long storeId;
}
