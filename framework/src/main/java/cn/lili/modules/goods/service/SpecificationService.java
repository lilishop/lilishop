package cn.lili.modules.goods.service;


import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.dos.Specification;
import cn.lili.modules.goods.entity.dto.SpecificationSearchParams;
import cn.lili.modules.goods.entity.vos.GoodsSpecValueVO;
import cn.lili.modules.goods.entity.vos.SpecificationVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;
import java.util.Map;

/**
 * 规格业务层
 *
 * @author pikachu
 * @date 2020-02-18 15:18:56
 */
public interface SpecificationService extends IService<Specification> {

    /**
     * 查询规格信息列表
     *
     * @param param 查询参数
     * @return 规格列表
     */
    List<SpecificationVO> getSpecList(Map<String, Object> param);

    /**
     * 根据分类id获取规格信息
     *
     * @param categoryId 分类id
     * @return 商品规格值列表
     */
    List<GoodsSpecValueVO> getGoodsSpecValue(String categoryId);

    /**
     * 获取规格详情
     *
     * @param id 规格ID
     * @return 规格详情
     */
    Specification getSpecification(String id);

    /**
     * 获取规格分页
     *
     * @param searchParams 搜索参数
     * @param pageVo       分页参数
     * @return 规格分页
     */
    IPage<SpecificationVO> getSpecificationPage(SpecificationSearchParams searchParams, PageVO pageVo);

    /**
     * 获取规格分页
     *
     * @param searchParams 搜索参数
     * @param pageVo       分页参数
     * @return 规格分页
     */
    IPage<Specification> getSpecificationByPage(SpecificationSearchParams searchParams, PageVO pageVo);

    /**
     * 添加规格
     *
     * @param specificationVO 规格信息
     * @return 是否添加成功
     */
    Specification addSpecification(SpecificationVO specificationVO);

    /**
     * 修改规格
     *
     * @param specificationVO 规格信息
     * @return 是否修改成功
     */
    boolean updateSpecification(SpecificationVO specificationVO);

    /**
     * 删除规格
     *
     * @param ids 规格ID
     * @return 是否删除成功
     */
    boolean deleteSpecification(List<String> ids);

}