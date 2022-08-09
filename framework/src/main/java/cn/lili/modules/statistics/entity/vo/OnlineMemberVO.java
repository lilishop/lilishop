package cn.lili.modules.statistics.entity.vo;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Date;

/**
 * 在线会员
 *
 * @author Chopper
 * @since 2021-02-21 09:59
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
public class OnlineMemberVO {

    /**
     * 在线时间
     */
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH")
    private Date date;

    /**
     * 在线会员人数
     */
    private Integer num;

    /**
     * 上一周期在线会员人数
     */
    private Integer lastNum;

}
