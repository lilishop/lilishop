package cn.lili.modules.order.order.service;

import cn.lili.common.vo.PageVO;
import cn.lili.modules.order.order.entity.dos.OrderComplaintCommunication;
import cn.lili.modules.order.order.entity.vo.OrderComplaintCommunicationSearchParams;
import cn.lili.modules.order.order.entity.vo.OrderComplaintCommunicationVO;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.service.IService;

/**
 * 订单投诉通信业务层
 *
 * @author paulG
 * @since 2020/12/5
 **/
public interface OrderComplaintCommunicationService extends IService<OrderComplaintCommunication> {

    /**
     * 添加订单投诉通信
     *
     * @param communicationVO 投诉通信VO
     * @return 状态
     */
    boolean addCommunication(OrderComplaintCommunicationVO communicationVO);

    /**
     * 获取通信记录
     *
     * @param searchParams 参数
     * @param pageVO       分页
     * @return
     */
    IPage<OrderComplaintCommunication> getCommunication(OrderComplaintCommunicationSearchParams searchParams, PageVO pageVO);


}
