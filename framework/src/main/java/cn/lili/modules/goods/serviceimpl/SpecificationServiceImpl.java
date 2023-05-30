package cn.lili.modules.goods.serviceimpl;

import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.modules.goods.entity.dos.CategorySpecification;
import cn.lili.modules.goods.entity.dos.Specification;
import cn.lili.modules.goods.mapper.SpecificationMapper;
import cn.lili.modules.goods.service.CategorySpecificationService;
import cn.lili.modules.goods.service.SpecificationService;
import com.baomidou.mybatisplus.core.conditions.query.QueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

/**
 * 商品规格业务层实现
 *
 * @author pikachu
 * @since 2020-02-18 16:18:56
 */
@Service
public class SpecificationServiceImpl extends ServiceImpl<SpecificationMapper, Specification> implements SpecificationService {

    /**
     * 分类-规格绑定
     */
    @Autowired
    private CategorySpecificationService categorySpecificationService;
    /**
     * 分类
     */
    @Autowired
    private CategoryServiceImpl categoryService;


    @Override
    public boolean deleteSpecification(List<String> ids) {
        boolean result = false;
        for (String id : ids) {
            //如果此规格绑定分类则不允许删除
            List<CategorySpecification> list = categorySpecificationService.list(new QueryWrapper<CategorySpecification>().eq("specification_id", id));
            if (!list.isEmpty()) {
                List<String> categoryIds = new ArrayList<>();
                list.forEach(item -> categoryIds.add(item.getCategoryId()));
                throw new ServiceException(ResultCode.SPEC_DELETE_ERROR,
                        JSONUtil.toJsonStr(categoryService.getCategoryNameByIds(categoryIds)));
            }
            //删除规格
            result = this.removeById(id);
        }
        return result;
    }

}