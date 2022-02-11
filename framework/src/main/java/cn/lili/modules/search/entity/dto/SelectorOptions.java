package cn.lili.modules.search.entity.dto;

import lombok.Data;

import java.util.List;

/**
 * @author paulG
 * @since 2020/10/20
 **/
@Data
public class SelectorOptions {

    private String name;

    private String value;

    private String url;

    private List<SelectorOptions> otherOptions;


}
