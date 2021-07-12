package cn.lili.modules.promotion.service;


import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dos.KanJiaActivity;
import cn.lili.modules.promotion.entity.dos.KanJiaActivityLog;
import cn.lili.modules.promotion.entity.dto.KanJiaActivityGoodsDTO;
import cn.lili.modules.promotion.entity.dto.KanJiaActivityQuery;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;


/**
 * 砍价活动参与记录业务层
 *
 * @author qiuqiu
 * @date 2021/7/1 9:45 上午
 */
public interface KanJiaActivityService extends IService<KanJiaActivity> {

    /**
     * 发起人发起砍价活动
     *
     * @param skuId 商品skuId
     * @return
     */
    KanJiaActivityLog add(String skuId);

    /**
     * 帮砍
     *
     * @param kanJiaActivityId 活动id
     * @return 砍价详细
     */
    KanJiaActivityLog helpKanJia(String kanJiaActivityId);

    /**
     * 根据条件查询我参与的砍价活动
     *
     * @param kanJiaActivityQuery 砍价活动查询条件
     * @param page                分页对象
     * @return 我参与的砍价活动信息
     */
    IPage<KanJiaActivity> getForPage(KanJiaActivityQuery kanJiaActivityQuery, PageVO page);


}