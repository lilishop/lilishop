package cn.lili.modules.search.entity.vo;

import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.AllArgsConstructor;
import lombok.Data;

import java.util.Date;

/**
 * 在线会员
 *
 * @author Chopper
 * @since 2021-02-21 09:59
 */
@Data
public class HotWordsHistoryVO {

    /**
     * 时间
     */
    private Date createTime;

    /**
     * 词
     */
    private String keywords;

    /**
     * 分数
     */
    private Integer score;

}
