package cn.lili.modules.member.service;

import cn.lili.modules.member.entity.dos.MemberNoticeSenter;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 会员消息业务层
 *
 * @author Chopper
 * @since 2020/11/17 3:44 下午
 */
public interface MemberNoticeSenterService extends IService<MemberNoticeSenter> {

    /**
     * 自定义保存方法
     *
     * @param memberNoticeSenter 会员消息
     * @return 操作状态
     */
    boolean customSave(MemberNoticeSenter memberNoticeSenter);

}