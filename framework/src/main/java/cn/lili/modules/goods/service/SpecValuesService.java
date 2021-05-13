package cn.lili.modules.goods.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.goods.entity.dos.SpecValues;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;


/**
 * 规格项接口
 *
 * @author pikachu
 * @date 2020-02-18 15:18:56
 */
public interface SpecValuesService extends IService<SpecValues> {

    /**
     * 保存规格值
     *
     * @param specId    规格Id
     * @param valueList 规格值集合
     */
    List<SpecValues> saveSpecValue(String specId, String[] valueList);

    /**
     * 保存规格值
     *
     * @param specId    规格Id
     * @param valueList 规格值集合
     */
    List<SpecValues> addSpecValue(String specId, String[] valueList);

    /**
     * 根据规格id查询规格值信息
     *
     * @param specIds 规格值ids
     * @return
     */
    List<SpecValues> getSpecValues(List<String> specIds);

    /**
     * 根据值获取规格值信息
     * 如果不存在则自动创建
     *
     * @param specValue 规格值
     * @param specId    规格ID
     * @return 规格值信息
     */
    SpecValues getSpecValues(String specValue, String specId);

    /**
     * 分页获取规格值
     *
     * @param specId  规格项ID
     * @param specVal 规格值
     * @param pageVo  分页参数
     * @return 规格值列表
     */
    IPage<SpecValues> queryByParams(String specId, String specVal, PageVO pageVo);

}