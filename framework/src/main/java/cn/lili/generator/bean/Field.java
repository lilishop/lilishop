package cn.lili.generator.bean;

import lombok.Data;

import java.math.BigDecimal;

/**
 * @author lili
 */
@Data
public class Field {

    private String field;

    private String name;

    private String level;

    private Boolean tableShow;

    private BigDecimal sortOrder;

    private Boolean searchable;

    private Boolean editable;

    private String type;

    private Boolean validate;

    private String searchType;

    private String searchLevel;

    private Boolean sortable;

    private Boolean defaultSort;

    private String defaultSortType;
}
