package cn.lili.modules.search.entity.dto;

import lombok.Data;

import java.util.List;

/**
 * 参数属性选择器
 *
 * @author paulG
 * @since 2020/10/20
 **/
@Data
public class ParamOptions {

    private String key;

    private List<String> values;

}
