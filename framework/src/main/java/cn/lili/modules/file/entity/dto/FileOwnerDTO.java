package cn.lili.modules.file.entity.dto;

import cn.lili.common.utils.DateUtil;
import cn.lili.common.vo.PageVO;
import com.alipay.api.internal.util.StringUtils;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;

import java.util.Calendar;
import java.util.Date;

/**
 * 文件查询所属者参数对象
 *
 * @author Chopper
 * @since 2021-02-22 17:20
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@EqualsAndHashCode(callSuper = false)
public class FileOwnerDTO extends PageVO {

    @Schema(description = "拥有者id")
    private String ownerId;

    @Schema(description = "拥有者名称")
    private String ownerName;

    @Schema(description = "用户类型")
    private String userEnums;

    @Schema(description = "原文件名")
    private String name;

    @Schema(description = "存储文件名")
    private String fileKey;

    @Schema(description = "文件类型")
    private String fileType;

    @Schema(description = "文件分类：IMAGE/VIDEO/FILE")
    private String fileCategory;

    @Schema(description = "文件夹ID")
    private String fileDirectoryId;

    @Schema(description = "起始日期")
    private String startDate;

    @Schema(description = "结束日期")
    private String endDate;

    public Date getConvertStartDate() {
        if (StringUtils.isEmpty(startDate)) {
            return null;
        }
        return DateUtil.toDate(startDate, DateUtil.STANDARD_DATE_FORMAT);
    }

    public Date getConvertEndDate() {
        if (StringUtils.isEmpty(endDate)) {
            return null;
        }
        //结束时间等于结束日期+1天 -1秒，
        Date date = DateUtil.toDate(endDate, DateUtil.STANDARD_DATE_FORMAT);
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(date);
        calendar.set(Calendar.DAY_OF_MONTH, calendar.get(Calendar.DAY_OF_MONTH) + 1);
        calendar.set(Calendar.SECOND, -1);
        return calendar.getTime();
    }

}
