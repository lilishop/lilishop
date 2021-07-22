package cn.lili.modules.goods.service;


import cn.lili.modules.goods.entity.dos.Specification;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;

/**
 * 规格业务层
 *
 * @author pikachu
 * @since 2020-02-18 15:18:56
 */
public interface SpecificationService extends IService<Specification> {

    /**
     * 删除规格
     *
     * @param ids 规格ID
     * @return 是否删除成功
     */
    boolean deleteSpecification(List<String> ids);

}