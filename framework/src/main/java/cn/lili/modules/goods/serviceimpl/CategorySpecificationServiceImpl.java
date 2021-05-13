package cn.lili.modules.goods.serviceimpl;

import cn.lili.modules.goods.entity.dos.CategorySpecification;
import cn.lili.modules.goods.entity.vos.CategorySpecificationVO;
import cn.lili.modules.goods.mapper.CategorySpecificationMapper;
import cn.lili.modules.goods.service.CategorySpecificationService;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Arrays;
import java.util.List;

/**
 * 商品分类规格业务层实现
 *
 * @author pikachu
 * @date 2020-02-23 15:18:56
 */
@Service
@Transactional
@RequiredArgsConstructor(onConstructor = @__(@Autowired))
public class CategorySpecificationServiceImpl extends ServiceImpl<CategorySpecificationMapper, CategorySpecification> implements CategorySpecificationService {

    private final CategorySpecificationMapper categorySpecificationMapper;

    @Override
    public List<CategorySpecificationVO> getCategorySpecList(String categoryId) {
        return categorySpecificationMapper.getCategorySpecList(categoryId);
    }

    @Override
    public List<CategorySpecification> getCategorySpecList(String[] categoryId) {
        return this.list(new LambdaQueryWrapper<CategorySpecification>().in(CategorySpecification::getCategoryId, Arrays.asList(categoryId)));
    }

    @Override
    public void deleteByCategoryId(String categoryId) {
        categorySpecificationMapper.delete(new LambdaQueryWrapper<CategorySpecification>().eq(CategorySpecification::getCategoryId,categoryId));
    }
}