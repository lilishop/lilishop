package cn.lili.security;

import cn.hutool.core.util.StrUtil;
import cn.lili.common.cache.Cache;
import cn.lili.common.cache.CachePrefix;
import cn.lili.common.security.AuthUser;
import cn.lili.common.security.enums.SecurityEnum;
import cn.lili.common.security.enums.UserEnums;
import cn.lili.common.token.SecretKeyUtil;
import cn.lili.common.utils.ResponseUtil;
import com.google.gson.Gson;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.ExpiredJwtException;
import io.jsonwebtoken.Jwts;
import lombok.SneakyThrows;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.www.BasicAuthenticationFilter;

import javax.naming.NoPermissionException;
import javax.servlet.FilterChain;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @author Chopper
 */
@Slf4j
public class ManagerAuthenticationFilter extends BasicAuthenticationFilter {

    private final Cache cache;

    public ManagerAuthenticationFilter(AuthenticationManager authenticationManager,
                                       Cache cache) {
        super(authenticationManager);
        this.cache = cache;
    }

    @SneakyThrows
    @Override
    protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain chain) {

        //从header中获取jwt
        String jwt = request.getHeader(SecurityEnum.HEADER_TOKEN.getValue());
        //如果没有token 则return
        if (StrUtil.isBlank(jwt)) {
            chain.doFilter(request, response);
            return;
        }

        //获取用户信息，存入context
        UsernamePasswordAuthenticationToken authentication = getAuthentication(jwt, response);
        //自定义权限过滤
        customAuthentication(request, response, authentication);
        SecurityContextHolder.getContext().setAuthentication(authentication);
        chain.doFilter(request, response);
    }

    /**
     * 自定义权限过滤
     *
     * @param request
     * @param authentication
     */
    private void customAuthentication(HttpServletRequest request, HttpServletResponse response, UsernamePasswordAuthenticationToken authentication) throws NoPermissionException {
        AuthUser authUser = (AuthUser) authentication.getDetails();
        Map<String, List<String>> permission = (Map<String, List<String>>) cache.get(CachePrefix.PERMISSION_LIST.getPrefix(UserEnums.MANAGER) + authUser.getId());
        if (authUser.getIsSuper()) {
            return;
        } else {
            //用户是否拥有权限判定œ
            //获取数据权限
//           if (request.getMethod().equals(RequestMethod.GET.name())) {
//               if (!PatternMatchUtils.simpleMatch(permission.get(PermissionEnum.SUPER).toArray(new String[0]), request.getRequestURI()) ||
//                       PatternMatchUtils.simpleMatch(permission.get(PermissionEnum.QUERY).toArray(new String[0]), request.getRequestURI())) {
//
//                   ResponseUtil.output(response, ResponseUtil.resultMap(false, 401, "抱歉，您没有访问权限"));
//                   throw new NoPermissionException("权限不足");
//               }
//           } else {
//               if (!PatternMatchUtils.simpleMatch(permission.get(PermissionEnum.SUPER).toArray(new String[0]), request.getRequestURI())) {
//
//                   ResponseUtil.output(response, ResponseUtil.resultMap(false, 401, "抱歉，您没有访问权限"));
//                   throw new NoPermissionException("权限不足");
//               }
//           }
            return;
        }
    }

    /**
     * 获取token信息
     *
     * @param jwt
     * @param response
     * @return
     */
    private UsernamePasswordAuthenticationToken getAuthentication(String jwt, HttpServletResponse response) {

        try {
            Claims claims
                    = Jwts.parser()
                    .setSigningKey(SecretKeyUtil.generalKeyByDecoders())
                    .parseClaimsJws(jwt).getBody();
            //获取存储在claims中的用户信息
            String json = claims.get(SecurityEnum.USER_CONTEXT.getValue()).toString();
            AuthUser authUser = new Gson().fromJson(json, AuthUser.class);

            //校验redis中是否有权限
            if (cache.hasKey(CachePrefix.ACCESS_TOKEN.getPrefix(UserEnums.MANAGER) + jwt)) {
                //用户角色
                List<GrantedAuthority> auths = new ArrayList<>();
                auths.add(new SimpleGrantedAuthority("ROLE_" + authUser.getRole().name()));
                //构造返回信息
                UsernamePasswordAuthenticationToken authentication = new UsernamePasswordAuthenticationToken(authUser.getUsername(), null, auths);
                authentication.setDetails(authUser);
                return authentication;
            }
            ResponseUtil.output(response, 403, ResponseUtil.resultMap(false, 403, "登录已失效，请重新登录"));
            return null;
        } catch (ExpiredJwtException e) {
            log.debug("user analysis exception:", e);
        } catch (Exception e) {
            log.error("user analysis exception:", e);
        }
        return null;
    }
}

