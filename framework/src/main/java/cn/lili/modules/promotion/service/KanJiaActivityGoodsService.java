package cn.lili.modules.promotion.service;


import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dos.KanJiaActivityGoods;
import cn.lili.modules.promotion.entity.dto.KanJiaActivityGoodsDTO;
import cn.lili.modules.promotion.entity.vos.KanJiaActivityGoodsParams;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.List;


/**
 * 砍价业务层
 *
 * @author qiuqiu
 * @date 2021/7/1 9:45 上午
 */
public interface KanJiaActivityGoodsService extends IService<KanJiaActivityGoods> {


    /**
     * 添加砍价活动商品
     *
     * @param kanJiaActivityGoodsDTOS 砍价商品
     * @return 是否添加成功
     */
    boolean add(List<KanJiaActivityGoodsDTO> kanJiaActivityGoodsDTOS);

    /**
     * 查询砍价活动商品分页信息
     *
     * @param kanJiaActivityGoodsParams 砍价活动商品
     * @param pageVO                    分页信息
     * @return 砍价商品
     */
    IPage<KanJiaActivityGoods> getForPage(KanJiaActivityGoodsParams kanJiaActivityGoodsParams, PageVO pageVO);


}