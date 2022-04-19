package cn.lili.modules.search.entity.dos;

import cn.lili.mybatis.BaseEntity;
import cn.lili.mybatis.BaseIdEntity;
import com.baomidou.mybatisplus.annotation.FieldFill;
import com.baomidou.mybatisplus.annotation.TableField;
import com.baomidou.mybatisplus.annotation.TableName;
import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModelProperty;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.elasticsearch.annotations.DateFormat;
import org.springframework.data.elasticsearch.annotations.Field;
import org.springframework.data.elasticsearch.annotations.FieldType;
import org.springframework.format.annotation.DateTimeFormat;

import java.util.Date;

/**
 * HotWordsHistory
 *
 * @author Chopper
 * @version v1.0
 * 2022-04-14 09:39
 */
@Data
@AllArgsConstructor
@NoArgsConstructor
@TableName("li_hot_words_history")
public class HotWordsHistory extends BaseIdEntity {

    /**
     * 词
     */
    private String keywords;

    /**
     * 分数
     */
    private Integer score;

    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm:ss")
    @DateTimeFormat(pattern = "yyyy-MM-dd")
    private Date createTime;
}
