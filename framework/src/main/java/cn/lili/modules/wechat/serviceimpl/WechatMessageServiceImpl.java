package cn.lili.modules.wechat.serviceimpl;

import cn.hutool.http.HttpUtil;
import cn.hutool.json.JSONObject;
import cn.hutool.json.JSONUtil;
import cn.lili.common.enums.ClientTypeEnum;
import cn.lili.common.enums.ResultCode;
import cn.lili.common.exception.ServiceException;
import cn.lili.common.utils.HttpUtils;
import cn.lili.modules.order.order.entity.enums.OrderStatusEnum;
import cn.lili.modules.wechat.entity.dos.WechatMessage;
import cn.lili.modules.wechat.entity.enums.WechatMessageItemEnums;
import cn.lili.modules.wechat.mapper.WechatMessageMapper;
import cn.lili.modules.wechat.service.WechatMessageService;
import cn.lili.modules.wechat.util.WechatAccessTokenUtil;
import cn.lili.modules.wechat.util.WechatMessageUtil;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * 微信模版消息（公众号） 业务实现
 *
 * @author Chopper
 */
@Service
@Slf4j
public class WechatMessageServiceImpl extends ServiceImpl<WechatMessageMapper, WechatMessage> implements WechatMessageService {

    @Autowired
    private WechatAccessTokenUtil wechatAccessTokenUtil;

    /**
     * 设置行业
     */
    private final String setIndustry = "https://api.weixin.qq.com/cgi-bin/template/api_set_industry?access_token=";
    /**
     * get 获取所有的模版
     */
    private final String allMsgTpl = "https://api.weixin.qq.com/cgi-bin/template/get_all_private_template?access_token=";
    /**
     * post 删除模版 添加模版 获取模版id
     */
    private final String delMsgTpl = "https://api.weixin.qq.com/cgi-bin/template/del_private_template?access_token=";
    /**
     * post 添加模版
     */
    private final String addTpl = "https://api.weixin.qq.com/cgi-bin/template/api_add_template?access_token=";

    @Override
    public void init() {
        try {
            this.baseMapper.deleteAll();

            //获取token
            String accessToken = wechatAccessTokenUtil.cgiAccessToken(ClientTypeEnum.H5);


            //设置行业
            Map<String, Object> setIndustryParams = new HashMap<>(16);
            //互联网/电子商务
            setIndustryParams.put("industry_id1", 1);
            //通信与运营商
            setIndustryParams.put("industry_id2", 5);
            String context = HttpUtils.doPostWithJson(setIndustry + accessToken, setIndustryParams);

            log.info("设置模版请求{},设置行业响应：{}", setIndustryParams, context);
            //获取已有模版，删除
            context = HttpUtil.get(allMsgTpl + accessToken);
            JSONObject jsonObject = new JSONObject(context);


            log.info("获取全部模版：{}", context);
            WechatMessageUtil.wechatHandler(jsonObject);
            List<String> oldList = new ArrayList<>();
            if (jsonObject.containsKey("template_list")) {
                jsonObject.getJSONArray("template_list").forEach(item -> {
                    oldList.add(JSONUtil.parseObj(item).getStr("template_id"));
                });
            }
            if (oldList.size() != 0) {
                oldList.forEach(templateId -> {
                    Map<String, Object> params = new HashMap<>(1);
                    params.put("template_id", templateId);
                    String message = WechatMessageUtil.wechatHandler(HttpUtil.post(delMsgTpl + accessToken, params));
                    log.info("删除模版请求:{},删除模版响应：{}", params, message);
                });
            }

            //加入数据
            List<WechatMessageData> tmpList = initData();
            tmpList.forEach(tplData -> {
                WechatMessage wechatMessage = new WechatMessage();
                Map<String, Object> params = new HashMap<>(1);
                params.put("template_id_short", tplData.getMsgId());
                String message = HttpUtils.doPostWithJson(addTpl + accessToken, params);
                log.info("添加模版请求:{},添加模版响应：{}", params, message);

                JSONObject tplContent = new JSONObject(message);
                WechatMessageUtil.wechatHandler(tplContent);

                //如果包含模版id则进行操作，否则抛出异常
                if (tplContent.containsKey("template_id")) {
                    wechatMessage.setCode(tplContent.getStr("template_id"));
                } else {
                    throw new ServiceException(ResultCode.WECHAT_MP_MESSAGE_TMPL_ERROR);
                }

                wechatMessage.setName(tplData.getName());
                wechatMessage.setFirst(tplData.getFirst());
                wechatMessage.setRemark(tplData.getRemark());
                wechatMessage.setKeywords(tplData.getKeyWord());
                wechatMessage.setEnable(true);
                wechatMessage.setOrderStatus(tplData.getOrderStatus().name());
                this.save(wechatMessage);
            });
        } catch (Exception e) {
            log.error("初始化微信消息异常", e);
        }
    }


    /**
     * 初始化数据
     *
     * @return
     */
    private List<WechatMessageData> initData() {
        List<WechatMessageData> msg = new ArrayList<>();
        //新订单消息提示
        msg.add(new WechatMessageData(
                "待支付",
                "您有新订单需要支付",
                "如有问题，请联系在线客服",
                "OPENTM207498902",
                WechatMessageItemEnums.MEMBER_NAME.name() + "," + WechatMessageItemEnums.ORDER_SN.name() + "," +
                        WechatMessageItemEnums.PRICE.name() + "," + WechatMessageItemEnums.GOODS_INFO.name(),
                OrderStatusEnum.UNPAID));
        //已发货
        msg.add(new WechatMessageData(
                "订单发货",
                "您的订单已发货",
                "如有问题，请联系在线客服",
                "OPENTM200565259",
                WechatMessageItemEnums.ORDER_SN.name() + "," +
                        WechatMessageItemEnums.LOGISTICS_NAME.name() + "," + WechatMessageItemEnums.LOGISTICS_NO.name(),
                OrderStatusEnum.DELIVERED));

        //已完成
        msg.add(new WechatMessageData(
                "订单完成",
                "您的订单已完成，是否有什么想对掌柜说的话呢",
                "诚邀您来评价，评价还赠送积分哦",
                "OPENTM416131050",
                WechatMessageItemEnums.MEMBER_NAME.name() + "," + WechatMessageItemEnums.ORDER_SN.name() + "," +
                        WechatMessageItemEnums.PRICE.name() + "," + WechatMessageItemEnums.GOODS_INFO.name(),
                OrderStatusEnum.COMPLETED));

        return msg;
    }

}

@Data
@AllArgsConstructor
@NoArgsConstructor
class WechatMessageData {

    /**
     * 名称
     */
    String name;
    /**
     * 首部信息
     */
    String first;
    /**
     * 备注信息
     */
    String remark;
    /**
     * 微信消息id
     */
    String msgId;

    /**
     * 消息内容
     */
    String keyWord;

    /**
     * 处于什么状态发送
     */
    OrderStatusEnum orderStatus;


}