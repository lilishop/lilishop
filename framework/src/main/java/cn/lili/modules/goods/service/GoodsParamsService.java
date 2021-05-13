package cn.lili.modules.goods.service;

import cn.lili.modules.goods.entity.dos.GoodsParams;
import cn.lili.modules.goods.entity.vos.GoodsParamsGroupVO;
import cn.lili.modules.goods.entity.vos.GoodsParamsVO;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;


/**
 * 商品关联参数业务层
 *
 * @author pikachu
 * @date 2020-03-13 16:18:56
 */
public interface GoodsParamsService extends IService<GoodsParams> {

    /**
     * 添加商品参数
     * @param paramList 参数列表
     * @param goodsId 商品ID
     */
    void addParams(List<GoodsParams> paramList, String goodsId);

    /**
     * 获取商品关联参数
     *
     * @param goodsId 商品ID
     * @return 商品关联参数
     */
    List<GoodsParams> getGoodsParamsByGoodsId(String goodsId);

    /**
     * 添加商品参数
     *
     * @param goodsParamsVO 商品参数
     * @return 添加是否成功
     */
    boolean addParams(GoodsParamsVO goodsParamsVO);

    /**
     * 根据分类id查询绑定参数信息
     *
     * @param categoryId 分类id
     * @param goodsId    商品id
     * @return 分类id
     */
    List<GoodsParamsVO> paramList(String categoryId, String goodsId);

    /**
     * 查询商品参数信息
     *
     * @param goodsId    商品id
     * @param categoryId 分了id
     * @return 商品参数信息
     */
    List<GoodsParamsGroupVO> queryGoodsParams(String goodsId, String categoryId);


}