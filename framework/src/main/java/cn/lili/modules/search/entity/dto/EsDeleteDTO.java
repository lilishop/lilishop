package cn.lili.modules.search.entity.dto;

import lombok.*;

import java.util.List;
import java.util.Map;

/**
 * ES删除DTO
 * 用于封装删除ES的参数
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class EsDeleteDTO {

    /**
     * 删除的索引
     */
    private List<String> ids;

    /**
     * 查询字段
     */
    private Map<String, Object> queryFields;

    /**
     * 删除的索引
     */
    @NonNull
    private Class<?> clazz;

}
