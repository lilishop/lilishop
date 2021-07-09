package cn.lili.modules.permission.entity.vo;

import cn.lili.common.utils.ObjectUtil;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;

import java.io.Serializable;
import java.util.Date;
import java.util.Map;


/**
 * 日志
 *
 * @author Chopper
 * @date 2020/12/2 17:50
 */
@Data
public class SystemLogVO implements Serializable {


    private static final long serialVersionUID = -8995552592401630086L;


    @ApiModelProperty(value = "id")
    private String id;


    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @ApiModelProperty(value = "日志记录时间")
    private Date createTime = new Date();

    @ApiModelProperty(value = "请求用户")
    private String username;

    @ApiModelProperty(value = "请求路径")
    private String requestUrl;

    @ApiModelProperty(value = "请求参数")
    private String requestParam;
    @ApiModelProperty(value = "响应参数")
    private String responseBody;

    @ApiModelProperty(value = "ip")
    private String ip;

    @ApiModelProperty(value = "方法操作名称")
    private String name;


    @ApiModelProperty(value = "请求类型")
    private String requestType;


    @ApiModelProperty(value = "自定义日志内容")
    private String customerLog;


    @ApiModelProperty(value = "ip信息")
    private String ipInfo;

    @ApiModelProperty(value = "花费时间")
    private Integer costTime;

    @ApiModelProperty(value = "商家")
    private Long storeId = -1L;

    /**
     * 转换请求参数为Json
     *
     * @param paramMap
     */
    public void setMapToParams(Map<String, String[]> paramMap) {

        this.requestParam = ObjectUtil.mapToString(paramMap);
    }
}
