package cn.lili.modules.promotion.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.promotion.entity.dos.Pintuan;
import cn.lili.modules.promotion.entity.vos.PintuanMemberVO;
import cn.lili.modules.promotion.entity.vos.PintuanSearchParams;
import cn.lili.modules.promotion.entity.vos.PintuanShareVO;
import cn.lili.modules.promotion.entity.vos.PintuanVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

import java.util.Date;
import java.util.List;

/**
 * 拼图活动业务层
 *
 * @author Chopper
 * @date 2020/11/18 9:45 上午
 */
public interface PintuanService extends IService<Pintuan> {

    /**
     * 根据条件分页查询拼团活动列表
     *
     * @param param 拼团活动查询参数
     * @param page  分页参数
     * @return 拼团活动列表
     */
    IPage<Pintuan> getPintuanByPage(PintuanSearchParams param, PageVO page);

    /**
     * 获取当前拼团的会员
     *
     * @param pintuanId 拼图id
     * @return 当前拼团的会员列表
     */
    List<PintuanMemberVO> getPintuanMember(String pintuanId);

    /**
     * 从mongo中根据条件分页查询拼团活动列表
     *
     * @param param 拼团活动查询参数
     * @param page  分页参数
     * @return 拼团活动列表
     */
    IPage<PintuanVO> getPintuanByPageFromMongo(PintuanSearchParams param, PageVO page);

    /**
     * 从mongo中查询拼团活动详情
     *
     * @param id 拼团ID
     * @return 拼团活动详情
     */
    PintuanVO getPintuanByIdFromMongo(String id);

    /**
     * 从mysql中查询拼团活动详情
     *
     * @param id 拼团活动id
     * @return 拼团活动详情
     */
    Pintuan getPintuanById(String id);

    /**
     * 从mongo中根据条件查询拼团活动总数
     *
     * @param param 拼团活动查询参数
     * @return 总数
     */
    Long getPintuanByPageFromMongoCount(PintuanSearchParams param);

    /**
     * 拼团新增业务处理
     *
     * @param pintuan 拼团实体
     * @return 是否成功
     */
    boolean addPintuan(PintuanVO pintuan);

    /**
     * 拼团修改
     *
     * @param pintuan 拼团实体
     * @return 是否成功
     */
    boolean modifyPintuan(PintuanVO pintuan);

    /**
     * 开启拼团
     *
     * @param pintuanId 拼团活动编号
     * @param startTime 开始时间
     * @param endTime   结束时间
     * @return 是否成功
     */
    boolean openPintuan(String pintuanId, Date startTime, Date endTime);

    /**
     * 关闭拼团
     *
     * @param pintuanId 拼团活动编号
     * @return 是否成功
     */
    boolean closePintuan(String pintuanId);

    /**
     * 删除拼团
     *
     * @param pintuanId 拼团活动编号
     * @return 是否成功
     */
    boolean deletePintuan(String pintuanId);

    /**
     * 获取拼团分享信息
     *
     * @param parentOrderSn 拼团团长订单sn
     * @param skuId         商品skuId
     * @return 拼团分享信息
     */
    PintuanShareVO getPintuanShareInfo(String parentOrderSn, String skuId);


}