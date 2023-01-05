package cn.lili.modules.permission.entity.vo;

import cn.lili.common.utils.ObjectUtil;
import cn.lili.elasticsearch.EsSuffix;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;
import org.springframework.data.annotation.Id;
import org.springframework.data.elasticsearch.annotations.Document;
import org.springframework.data.elasticsearch.annotations.Field;
import org.springframework.data.elasticsearch.annotations.FieldType;

import java.io.Serializable;
import java.util.Date;
import java.util.Map;


/**
 * 日志
 *
 * @author Chopper
 * @since 2020/12/2 17:50
 */
@Data
@Document(indexName = "#{@elasticsearchProperties.indexPrefix}_" + EsSuffix.LOGS_INDEX_NAME)
@ToString
@NoArgsConstructor
@Accessors(chain = true)
public class SystemLogVO implements Serializable {


    private static final long serialVersionUID = -8995552592401630086L;

    @Id
    @ApiModelProperty(value = "id")
    private String id;


    @ApiModelProperty(value = "日志记录时间")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @Field(type = FieldType.Date, fielddata = true)
    private Date createTime = new Date();

    @ApiModelProperty(value = "请求用户")
    @Field(type = FieldType.Text)
    private String username;

    @ApiModelProperty(value = "请求路径")
    @Field(type = FieldType.Text)
    private String requestUrl;

    @ApiModelProperty(value = "请求参数")
    @Field(type = FieldType.Text)
    private String requestParam;

    @ApiModelProperty(value = "响应参数")
    @Field(type = FieldType.Text)
    private String responseBody;

    @ApiModelProperty(value = "ip")
    @Field(type = FieldType.Keyword)
    private String ip;

    @ApiModelProperty(value = "方法操作名称")
    @Field(type = FieldType.Keyword)
    private String name;


    @ApiModelProperty(value = "请求类型")
    @Field(type = FieldType.Keyword)
    private String requestType;


    @ApiModelProperty(value = "自定义日志内容")
    @Field(type = FieldType.Text)
    private String customerLog;


    @ApiModelProperty(value = "ip信息")
    @Field(type = FieldType.Text)
    private String ipInfo;

    @ApiModelProperty(value = "花费时间")
    @Field(type = FieldType.Integer)
    private Integer costTime;

    @ApiModelProperty(value = "商家")
    @Field(type = FieldType.Long)
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
