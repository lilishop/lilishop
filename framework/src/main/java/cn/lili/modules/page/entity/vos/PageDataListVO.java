package cn.lili.modules.page.entity.vos;

import cn.lili.common.enums.SwitchEnum;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * 楼层装修数据VO
 *
 * @author Bulbasaur
 * @since 2020/12/10 17:42
 */
@Data
public class PageDataListVO {

    @Schema(description = "页面ID")
    private String id;
    @Schema(description = "页面名称")
    private String name;
    /**
     * @see SwitchEnum
     */
    @Schema(description = "页面开关状态", allowableValues = "OPEN,CLOSE")
    private String pageShow;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "创建时间")
    private Date createTime;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    @Schema(description = "修改时间")
    private Date updateTime;
}
