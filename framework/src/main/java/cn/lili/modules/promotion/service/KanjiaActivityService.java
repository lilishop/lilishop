package cn.lili.modules.promotion.service;


import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dos.KanjiaActivity;
import cn.lili.modules.promotion.entity.dos.KanjiaActivityLog;
import cn.lili.modules.promotion.entity.dto.search.KanjiaActivityQuery;
import cn.lili.modules.promotion.entity.dto.search.KanjiaActivitySearchParams;
import cn.lili.modules.promotion.entity.vos.kanjia.KanjiaActivityVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;


/**
 * 砍价活动参与记录业务层
 *
 * @author qiuqiu
 * @date 2021/7/1 9:45 上午
 */
public interface KanjiaActivityService extends IService<KanjiaActivity> {

    /**
     * 获取砍价活动
     *
     * @param kanJiaActivitySearchParams 砍价活动搜索参数
     * @return 砍价活动
     */
    KanjiaActivity getKanjiaActivity(KanjiaActivitySearchParams kanJiaActivitySearchParams);

    /**
     * 获取砍价活动
     * <p>
     * 有值说明是已参加的砍价活动
     * 没有值说明是未参加的砍价活动
     *
     * @param kanJiaActivitySearchParams 砍价活动搜索参数
     * @return 砍价活动
     */
    KanjiaActivityVO getKanjiaActivityVO(KanjiaActivitySearchParams kanJiaActivitySearchParams);

    /**
     * 发起人发起砍价活动
     *
     * @param id 活动ID
     * @return
     */
    KanjiaActivityLog add(String id);

    /**
     * 帮砍
     *
     * @param kanJiaActivityId 活动id
     * @return 砍价详细
     */
    KanjiaActivityLog helpKanJia(String kanJiaActivityId);

    /**
     * 根据条件查询我参与的砍价活动
     *
     * @param kanJiaActivityQuery 砍价活动查询条件
     * @param page                分页对象
     * @return 我参与的砍价活动信息
     */
    IPage<KanjiaActivity> getForPage(KanjiaActivityQuery kanJiaActivityQuery, PageVO page);


    /**
     * 结束砍价活动
     *
     * @param kanjiaId 砍价活动id
     * @return 是否更新成功
     */
    boolean endKanjiaActivity(String kanjiaId);
}