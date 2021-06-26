package cn.lili.modules.promotion.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dos.Seckill;
import cn.lili.modules.promotion.entity.vos.SeckillSearchParams;
import cn.lili.modules.promotion.entity.vos.SeckillVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 秒杀业务层
 *
 * @author Chopper
 * @date 2020/11/18 9:45 上午
 */
public interface SeckillService extends IService<Seckill> {


    /**
     * 从mysql中根据条件获取秒杀活动分页列表
     *
     * @param queryParam 查询参数
     * @param pageVo     分页参数
     * @return 秒杀活动分页列表
     */
    IPage<Seckill> getSeckillByPageFromMysql(SeckillSearchParams queryParam, PageVO pageVo);

    /**
     * 从mongo中根据条件获取秒杀活动分页列表
     *
     * @param queryParam 查询参数
     * @param pageVo     分页参数
     * @return 秒杀活动分页列表
     */
    IPage<SeckillVO> getSeckillByPageFromMongo(SeckillSearchParams queryParam, PageVO pageVo);

    /**
     * 从mongo中获取秒杀活动信息
     *
     * @param id 秒杀活动id
     * @return 秒杀活动信息
     */
    SeckillVO getSeckillByIdFromMongo(String id);

    /**
     * 初始化秒杀活动，默认开启三十天的秒杀活动
     *
     * @return 是否保存成功
     */
    void init();
    /**
     * 保存秒杀活动
     *
     * @param seckill 秒杀活动
     * @return 是否保存成功
     */
    boolean saveSeckill(Seckill seckill);

    /**
     * 商家报名秒杀活动活动
     *
     * @param storeId   商家编号
     * @param seckillId 秒杀活动编号
     */
    void storeApply(String storeId, String seckillId);

    /**
     * 修改秒杀活动
     *
     * @param seckillVO 秒杀活动信息
     * @return 是否修改成功
     */
    boolean modifySeckill(SeckillVO seckillVO);

    /**
     * 删除秒杀活动
     *
     * @param id 秒杀活动编号
     */
    void deleteSeckill(String id);

    /**
     * 开启一个秒杀活动
     *
     * @param id 秒杀活动编号
     */
    void openSeckill(String id);

    /**
     * 关闭一个秒杀活动
     *
     * @param id 秒杀活动编号
     */
    void closeSeckill(String id);

    /**
     * 获取当前可参与的活动数量
     *
     * @return 可参与活动数量
     */
    Integer getApplyNum();

    /**
     * 更新秒杀活动的商品数量
     * @param seckillId 秒杀活动ID
     * @return 更新结果
     */
    void updateSeckillGoodsNum(String seckillId);
}