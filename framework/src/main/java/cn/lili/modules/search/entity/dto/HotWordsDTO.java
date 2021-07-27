package cn.lili.modules.search.entity.dto;

import lombok.Data;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;

/**
 * 搜索热词
 *
 * @author Chopper
 * @version v1.0
 * 2021-07-26 15:46
 */
@Data
public class HotWordsDTO {

    @NotBlank(message = "搜索热词不能为空")
    private String keywords;

    @NotNull(message = "分数不能为空")
    private Integer point;
}

